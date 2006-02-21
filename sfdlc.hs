module Main where

import List     (unfoldr, find, elemIndex)
import Maybe    (isJust, fromJust, fromMaybe, listToMaybe)
import System   (getArgs, exitWith, ExitCode(..), getProgName)

import IO ( stdin, stderr,
            hGetContents, hPrint, hFlush, hPutStrLn,
            openFile, hClose,
            IOMode(..),
            ioeGetErrorString)

import qualified Distribution.GetOpt    as Opt
import Data.IORef                       (IORef,newIORef,readIORef,writeIORef)
import System.IO.Unsafe                 (unsafePerformIO)


import SFDL.Abs                  -- the abstract syntax types from BNFC
import SFDL.Lex                  -- Alex lexer
import SFDL.Par                  -- the Happy parser 
import SFDL.Print                -- generated by BNFC, to print an AST

import SFDL.ErrM

import qualified Data.Map as Map

import qualified Intermediate   as Im
import qualified HoistStms      as Ho
import qualified Unroll         as Ur
import qualified CircGen        as CG
import qualified Runtime        as Run


import TypeChecker

import SashoLib

import qualified Text.PrettyPrint.HughesPJ as PP


main = do argv          <- getArgs
          let o@(opts,args,errs) = Opt.getOpt Opt.Permute optionControl argv
--          hPrint stderr o
          if (not $ null errs) then do name     <- getProgName
                                       hPutStrLn stderr "Command line errors:"
                                       mapM_ (hPutStrLn stderr) errs
                                       hPutStrLn stderr $ usage name
                                       exitWith ExitSuccess
                               else return ()
          if elemIndex Help opts /= Nothing then do name     <- getProgName
                                                    hPutStrLn stderr $ usage name
                                                    exitWith ExitSuccess
                                            else return ()
          let mb_infile = listToMaybe args

          writeIORef g_Flags (opts, mb_infile)

          hInfile       <- maybeM (return stdin)
                                  (\infile ->
                                       (openFile infile ReadMode)
                                           `catch`
                                       (\e -> do putStrLn $
                                                   "failed to open " ++
                                                   infile ++
                                                   ": " ++
                                                   ioeGetErrorString e
                                                 exitWith $ ExitFailure 2))
                                  mb_infile

          (hGetContents hInfile >>= run 1 pProg)



------------
-- command line argument processing stuff
------------
g_Flags :: IORef ([Flag],       -- flags
                  Maybe String) -- (optional) input file name
g_Flags = unsafePerformIO $ newIORef ([], Nothing)

data Flag 
    = Verbose  | Version | Help
    | Input String | Output (Maybe String) | LibDir String |
      Runfile (Maybe String)
      deriving (Eq,Show)
    
optionControl :: [Opt.OptDescr Flag]
optionControl =
    [ Opt.Option ['v']      ["verbose"] (Opt.NoArg Verbose)       "chatty output on stderr"
    , Opt.Option ['V','?']  ["version"] (Opt.NoArg Version)       "show version number"
    , Opt.Option ['o']      ["output"]  (Opt.OptArg Output "<file>")  "output circuit to <file>"
    , Opt.Option ['h']      ["help"]    (Opt.NoArg Help)            "Print help (this text)"
    , Opt.Option ['r']      ["run"]     (Opt.OptArg Runfile "<file>") "Run circuit into <file>"
     ]
usage name = Opt.usageInfo ("Usage: " ++ name ++ " <options> <input file>\n\
                                                 \Produces <output> and cct.gviz\n\
                                                 \Options:")
                           optionControl


-- extract the output file from the global flags
getOutFile = do (flags,_)   <- readIORef g_Flags
                return ( do (Output f) <- find isOut flags -- Maybe monad
                            return f )
    where isOut (Output _)  = True
          isOut _           = False

getRunFile = do (flags,_)   <- readIORef g_Flags
                return ( do (Runfile f) <- find isRunFile flags -- Maybe monad
                            return f )
    where isRunFile (Runfile _) = True
          isRunFile _           = False

getInFile = do (_,mb_infile) <- readIORef g_Flags
               return $ fromJustMsg "No input file specified"
                                    mb_infile


type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

{-
showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree
-}

--run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v parser input =
    let tokens  = myLexer input
        ast     = parser tokens
    in case ast of
         Bad s    -> do putStrLn "\nParse Failed...\n"
                        putStrV v "Tokens:"
                        putStrV v $ show tokens
                        putStrLn s
         Ok  prog@(Prog _ _) ->
           do putStrLn "\nParse Successful!"
              case typeCheck prog of
                (Left err)        -> print $ "Type Error! " << err
                (Right prog@(Im.Prog pname
                             Im.ProgTables {Im.types=typ_table,
                                            Im.funcs=fs}))      ->
                   do putStrLn "After typechecking:"
                      hPrint stderr prog
                      let prog_flat = Ho.flattenProg prog
                      putStrLn "Flattened program:"
                      hPrint stderr prog_flat
                      case Ur.unrollProg prog_flat of
                        (Left err)       -> print $ "Unrolling Error! " << err
                        (Right stms)     ->
                           do putStrLn "Unrolled main:"
                              hPrint stderr (PP.vcat (map Im.docStm stms))
                              infile         <- getInFile
                              mb_outfile_opt <- getOutFile
                              let mb_outfile = fromMaybe Nothing
                                                         mb_outfile_opt
                                  gatesFile = fromMaybe (modFileExt infile "cct")
                                                         mb_outfile
                              let cctFile      = "cct.gviz"
                                  args         = CG.extractInputs prog
                                  (cct,gates)  = CG.genCircuit typ_table stms args
--                              hPrint stderr cct; hFlush stderr
                              putStrLn $ "Now writing the circuit out to " ++ cctFile
                              writeFile cctFile (CG.showCct cct)
                              putStrLn $ "Writing the gate list to " ++ gatesFile
                              writeGates gatesFile gates
                              mb_runfile_opt <- getRunFile
                              maybeM (return ()) -- if no -r option given at all
                                     ( \f -> do let fname = fromMaybe (modFileExt infile "run")
                                                                      f
                                                putStrLn ("Now running the circuit into " ++ 
                                                          fname)
                                                h       <- openFile fname WriteMode
                                                vals    <- Run.formatRun gates []
                                                mapM_ (hPutStrLn h) vals
                                                hClose h )
                                     mb_runfile_opt

modFileExt file newext = let name = takeWhile (/= '.') file
                         in  name ++ "." ++ newext

--                                   mapM_ print cct

{-
                                                        (stms,errs') ->
--                                                          errs' = strictList errs
                                                            do print (PP.vcat (map Im.docStm stms))
                                                               print errs'
{-
                                                      if not $ null errs'
                                                         then putStrLn "Failed!" -- >> print errs'
                                                         else return ()
-}
-}

writeGates file gates = do h        <- openFile file WriteMode
                           mapM (hPrint h) gates
                           hClose h


printMap m = mapM_ print $ Map.fold (:) [] m

getMain (Im.Prog _ (Im.ProgTables {Im.funcs=fs})) =
    fromJust $ Map.lookup "main" fs
        
testFlatten (Im.Prog pname (Im.ProgTables {Im.funcs=fs})) fname =
    do let (Just f) = Map.lookup fname fs
           f_flat = Ho.flattenFunc f
       print f_flat

--                                 Prog _ decls -> case head decls of
--                                                      FunDecl _ _ _ _ stms -> print $ unrollStms stms
                                                 --                                                                    showTree v $ Fun TInt (Ident "unrolled") unrolled
