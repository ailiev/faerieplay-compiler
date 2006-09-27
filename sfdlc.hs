module Main where

import List     (intersect, sort)
import Maybe    (isJust, fromJust, fromMaybe, listToMaybe, maybeToList)
import System   (getArgs, exitWith, ExitCode(..), getProgName)

import IO ( Handle,
            stdin, stdout, stderr,
            hGetContents, hPrint, hFlush, hPutStrLn, hPutStr,
            openFile, hClose,
            IOMode(..),
            ioeGetErrorString)

import qualified Distribution.GetOpt    as Opt
import Data.IORef                       (IORef,newIORef,readIORef,writeIORef)
import System.IO.Unsafe                 (unsafePerformIO)

import qualified Data.Graph.Inductive.Graph as Gr
import qualified Data.Graph.Inductive.Tree  as TreeGr

import qualified Debug.Trace                as Trace


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
import qualified GraphLib       as GrLib
import           UDraw
import           Common

import TypeChecker

import SashoLib

-- NOTE: this is updated by emacs function time-stamp; see emacs "Local Variables:"
-- section at the end.
-- g_timestamp = "2006-09-25 20:02:43 sasho"

-- this updated by subversion
g_svn_id = "subversion $Revision$"
g_version = g_svn_id


data LogPrio = PROGRESS | INFO | DEBUG
   deriving (Show,Eq,Ord)

logmsg prio msg = hPutStrLn stderr $ show prio ++ ": " ++ msg


main = do argv          <- getArgs
          name          <- getProgName
          let o@(unsorted_opts,args,errs) = Opt.getOpt Opt.Permute optionControl argv
--          hPrint stderr o
          if (not $ null errs) then do hPutStrLn stderr "Command line errors:"
                                       mapM_ (hPutStrLn stderr) errs
                                       hPutStrLn stderr $ usage name
                                       exitWith ExitSuccess
                               else return ()

          -- bring the action to the front by sorting
          let (action:opts) = sort $ unsorted_opts

              mb_infile     = listToMaybe args
              infile        = fromMaybe "-" mb_infile

          -- get the input handle, a file or stdin
          hIn               <- openFH ReadMode infile

          -- get an output file name if an input was provided
          -- Maybe monad
          let mb_outfile    = do infile <- mb_infile
                                 return $
                                   modFileExt infile $
                                   case action of
                                     Compile    -> "runtime"
                                     Run        -> "run"
                                     MakeGraph  -> "udg" -- UDrawGraph
                                     PruneGraph _ _ -> "pruned-cct"
                                     _        -> fail "" -- no output file in this case

          -- output handle:
          -- in order try: an '-o' option, input file with changed suffix, stdout
          let fOut      = fromJust $ listToMaybe $ [f | Output f <- unsorted_opts] ++
                                                     (maybeToList mb_outfile) ++
                                                     ["-"]
          hOut          <- openFH WriteMode fOut

          case action of
            Compile         -> driveCompile infile hIn hOut opts
                               `trace` ("Compiling into " ++ fOut)
            Run             -> driveRunFile hIn hOut
            MakeGraph       -> driveGraph   hIn hOut
            PruneGraph s d  -> doPruneGraph s d hIn hOut

            Version         -> putStrLn $ name ++ " Version " ++ g_version
            Help            -> hPutStrLn stderr $ usage name

            -- by default, compile
            _               -> driveCompile infile hIn hOut opts

          mapM_ hClose [hIn, hOut]

          exitWith ExitSuccess

          -- save away the flags and input file name
--          writeIORef g_Flags (unsorted_opts, mb_infile)




driveCompile fIn hIn hOut opts  =
    do let fIn' = case fIn of
                           "-"  -> "a.sfdl" -- just so any intermediate files have a
                                            -- basename
                           _    -> fIn
       hGetContents hIn >>=
         doCompile 1 pProg fIn' (opts `intersect` cCOMPILE_EXTRAS) hOut



driveRunFile = doRunCct 

driveGraph = doWriteGraph


------------
-- command line argument processing stuff
------------
g_Flags :: IORef ([Flag],       -- flags
                  Maybe String) -- (optional) input file name
g_Flags = unsafePerformIO $ newIORef ([], Nothing)


data Flag 
    = 
      -- actions:
      Compile
    | MakeGraph
    | PruneGraph { start    :: Gr.Node,
                   d        :: Int } -- produce a cct file with just a part of the circuit, 'd'
                                     -- nodes going backwards from 'start'
    | Run

    | Version
    | Help

      -- options:
    | DumpGates                 -- dump the list of gates in Read/Show form
    | DumpGraph                 -- dump the bare graph in Read/Show form

    | Verbose
    | Output String
    deriving (Eq,Ord,Show)




-- possible extra files to generate during the compilation phase.
cCOMPILE_EXTRAS = [DumpGates, DumpGraph]
    

optionControl :: [Opt.OptDescr Flag]
optionControl = [
                 -- actions
      Opt.Option ['c']      ["compile"] (Opt.NoArg Compile)             "Compile SFDL (default action); \
                                                                        \.runtime"
    , Opt.Option ['G']      ["mkgraph"] (Opt.NoArg MakeGraph)           "generate a UDrawGraph file; .udg"
    , Opt.Option ['r']      ["run"]     (Opt.NoArg Run)                 "Run circuit; .run"
    , Opt.Option [   ]      ["prune"]   (Opt.ReqArg getPruneArgs
                                                    "<start node>,<path len>")
                                                                        "Prune the circuit; .cct"

    , Opt.Option ['V','?']  ["version"] (Opt.NoArg Version)             "Show version number"
    , Opt.Option ['h']      ["help"]    (Opt.NoArg Help)                "Print help (this text)"

                -- flags
    , Opt.Option [  ]       ["dump-gates",
                             "dgt"]     (Opt.NoArg DumpGates)           "dump the list of gates \
                                                                         \in Read/Show form; .gates"
    , Opt.Option [  ]       ["dump-graph",
                             "dgr"]     (Opt.NoArg DumpGraph)           "dump the graph in Read/Show form; \
                                                                        \.cct"

    , Opt.Option ['v']      ["verbose"] (Opt.NoArg Verbose)             "Chatty output on stderr"
    , Opt.Option ['o']      ["output"]  (Opt.ReqArg Output "<file>")    "Output to <file>"
     ]


getPruneArgs :: String -> Flag
getPruneArgs str = let (start,str1) = head $ reads str
                       (',':str2)   = str1
                       (dist,[])    = head $ reads str2
                   in
                     PruneGraph start dist

usage name = Opt.usageInfo ("Usage: " ++ name ++
                            " <options> in.sfdl\n\
                            \Compiles SFDL files, simulates a circuit, and manipulates circuit files.\n\
                            \Options, with associated default output filename extensions:")
                           optionControl

openFH mode name =  Trace.trace ("Opening file " ++ name ++ " in " ++ show mode) $
                    case name of
                      "-"   -> return $ case mode of WriteMode   -> stdout
                                                     ReadMode    -> stdin
                      _     -> openFile name mode
                               `catch`
                               (\e -> do hPutStrLn stderr $ "failed to open " ++ name ++ ": "
                                                      ++ ioeGetErrorString e
                                         exitWith $ ExitFailure 2)


-- extract various options from the global options list
getOutFile = do (flags,_)   <- readIORef g_Flags
                return $ listToMaybe [f | Output f <- flags]

getInFile = do (_,mb_infile) <- readIORef g_Flags
               return $ fromJustMsg "No actual input file specified"
                                    mb_infile


type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()


cCOMPILE_INTERMEDIATE_INFO = [ (DumpGraph, "cct"),
                               (DumpGates, "gates") ]

doCompile v parser filenameIn extras hOut strIn =
    let tokens  = myLexer strIn
        ast     = parser tokens
    in case ast of
         Bad s    -> do putStrLn "\nParse Failed...\n"
                        putStrV v "Tokens:"
                        putStrV v $ show tokens
                        putStrLn s
         Ok  prog@(Prog _ _) ->
           do logmsg INFO "Parse Successful!"
              case typeCheck prog of
                (Left err)        -> hPutStrLn stderr $ "Type Error! " << err
                (Right prog@(Im.Prog pname
                             Im.ProgTables {Im.types=typ_table,
                                            Im.funcs=fs}))      ->
                   do -- hPrint stderr prog
                      logmsg PROGRESS "Typechecking done"

                      let prog_flat = Ho.flattenProg prog
                      -- hPrint stderr prog_flat
                      logmsg PROGRESS "Flattened program"
                      case Ur.unrollProg prog_flat of
                        (Left err)       -> hPutStrLn stderr $ "Unrolling Error! " << err
                        (Right stms)     ->
                           do -- hPrint stderr (PP.vcat (map Im.docStm stms))
                              logmsg PROGRESS "Unrolled main; Starting to generate the circuit"

                              let -- compile the circuit
                                  args          = CG.extractInputs prog
                                  (cct,gates)   = CG.genCircuit typ_table stms args

                              -- write the .runtime file
                              mapM_ (hPutStr hOut . CG.cctShow) gates
                              logmsg PROGRESS "Wrote the circuit runtime file"

                              -- and the extra outputs requested
                              mapM_ (doExtras filenameIn (cct,gates)) extras


doExtras filenameIn ccts flag
    = do let outfile = modFileExt filenameIn
                                  (fromJustMsg "extra compile action"
                                               (lookup flag cCOMPILE_INTERMEDIATE_INFO))
         logmsg PROGRESS $ "Doing " ++ show flag ++ " into " ++ outfile
         writeFile outfile (doExtra flag ccts)
         logmsg PROGRESS $ "Done with " ++ show flag ++ "."


doExtra DumpGates (cct,gates)   = show gates
doExtra DumpGraph (cct,gates)   = GrLib.showGraph cct


readCct :: (Read a, Read b) => Handle -> IO (TreeGr.Gr a b)
readCct hIn = hGetContents hIn >>= readIO


doWriteGraph cct_fh hOut = do logmsg PROGRESS $ "Generating the circuit graph (UDrawGraph)"
                              cct     <- hGetContents cct_fh >>= readIO
                              -- NOTE: get an "Overlapping instances for Read" error if
                              -- the type of cct is not specified, could be solved by ghc
                              -- option -fallow-incoherent-instances which sounds bad.
                              let _     = (cct :: CG.Circuit)
                              hPutStrLn hOut $ CG.showCctGraph cct


doPruneGraph :: Gr.Node -> Int -> Handle -> Handle -> IO()
doPruneGraph start dist hIn hOut =
    do logmsg PROGRESS "Pruning the circuit graph"
       cct              <- hGetContents hIn >>= readIO
       let _            = cct :: CG.Circuit
       let cct_pruned   = GrLib.pruneGraph GrLib.back start dist cct
       (hPutStrLn hOut $ GrLib.showGraph cct_pruned)
         `trace` ("doPruneGraph: cct_pruned is\n" ++ show cct_pruned)


doRunCct gates_fh hOut = 
    do logmsg PROGRESS "Now running the circuit"
       gates    <- hGetContents gates_fh >>= readIO
       ins      <- Run.getUserInputs gates
       vals     <- Run.formatRun gates ins
       mapM_ (hPutStrLn hOut) vals

       
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


-- printMap m = mapM_ print $ Map.fold (:) [] m

getMain (Im.Prog _ (Im.ProgTables {Im.funcs=fs})) =
    fromJustMsg "Failed to find main function" $ Map.lookup "main" fs

        
testFlatten (Im.Prog pname (Im.ProgTables {Im.funcs=fs})) fname =
    do let (Just f) = Map.lookup fname fs
           f_flat = Ho.flattenFunc f
       print f_flat

--                                 Prog _ decls -> case head decls of
--                                                      FunDecl _ _ _ _ stms -> print $ unrollStms stms
                                                 --                                                                    showTree v $ Fun TInt (Ident "unrolled") unrolled


{-
Local Variables:
time-stamp-start:"g_timestamp[ 	]*=[ 	][\"]"
time-stamp-line-limit:60
End:
-}
