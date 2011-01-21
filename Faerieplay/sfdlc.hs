{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -cpp #-}

--
-- Circuit compiler for the Faerieplay hardware-assisted secure
-- computation project at Dartmouth College.
--
-- Copyright (C) 2003-2007, Alexander Iliev <sasho@cs.dartmouth.edu> and
-- Sean W. Smith <sws@cs.dartmouth.edu>
--
-- All rights reserved.
--
-- This code is released under a BSD license.
-- Please see LICENSE.txt for the full license and disclaimers.
--

module Main where

import List     (intersect, sort)
import Maybe    (isJust, fromJust, fromMaybe, listToMaybe, maybeToList)
import System   (getArgs, exitWith, ExitCode(..), getProgName, exitFailure)

import IO ( Handle,
            stdin, stdout, stderr,
            hGetContents, hPrint, hFlush, hPutStrLn, hPutStr,
            openFile, hClose,
            IOMode(..),
            ioeGetErrorString)

import qualified System.Console.GetOpt  as Opt


import Data.IORef                       (IORef,newIORef,readIORef,writeIORef)
import System.IO.Unsafe                 (unsafePerformIO)

import qualified Data.Graph.Inductive.Graph as Gr
import qualified Data.Graph.Inductive.Tree  as TreeGr

import qualified Debug.Trace                as Trace

import qualified Text.PrettyPrint           as PP

import qualified System.IO.Error            as IOErr

-- get the parser stuff; for SFDL syntax, or for C

#if defined SYNTAX_SFDL

import qualified Faerieplay.Bnfc.Sfdl.Abs   as SrcAbs -- the abstract syntax types from BNFC
import qualified Faerieplay.Bnfc.Sfdl.Par   as SrcPar -- the Happy parser 
import qualified Faerieplay.Bnfc.Sfdl.ErrM  as SrcErrM

import qualified Faerieplay.SFDL_Fixup      as SrcFixup         (fixupProg)

import qualified Faerieplay.GenHelper_SFDL  as SrcGenHelper     (genHelper)

#elif defined SYNTAX_C

-- for C:
import qualified Faerieplay.Bnfc.Fcpp.Abs       as SrcAbs -- the abstract syntax types from BNFC
import qualified Faerieplay.Bnfc.Fcpp.Par       as SrcPar -- the Happy parser 
import qualified Faerieplay.Bnfc.Fcpp.ErrM      as SrcErrM

import qualified Faerieplay.SFDL_C_Fixup        as SrcFixup      (fixupProg)

import qualified Faerieplay.GenHelper_C         as SrcGenHelper     (genHelper)

#else

#error No syntax defined

#endif


import qualified Data.Map as Map

import qualified Faerieplay.Version         as Vers


import qualified Faerieplay.Intermediate   as Im
import qualified Faerieplay.HoistStms      as Ho
import qualified Faerieplay.Unroll         as Ur
import qualified Faerieplay.CircGen        as CG
import qualified Faerieplay.Runtime        as Run
import qualified IlievUtils.GraphLib       as GrLib
import           IlievUtils.UDraw
import           Faerieplay.Common          (trace,cMINPRIO,LogPrio(..),RunFlag(..))
import Faerieplay.ErrorWithContext          (ErrorWithContext(..))

import Faerieplay.TypeChecker

import IlievUtils.Misc


cC_HELPER_TEMPLATE = "GenHelper_C.templ.cc"



logmsg prio msg = do if prio >= cMINPRIO
                      then hPutStrLn stderr $ "Main log " ++ show prio ++ ": " ++ msg
                      else return ()


main = do argv          <- getArgs
          name          <- getProgName
          let o@(given_opts,args,errs) = Opt.getOpt Opt.Permute optionControl argv
--          hPrint stderr o

          -- check for an error during option parsing.
          if (not $ null errs) then do hPutStrLn stderr "Command line errors:"
                                       mapM_ (hPutStrLn stderr) errs
                                       hPutStrLn stderr $ usage name
                                       exitWith ExitSuccess
                               else return ()

          -- if no options - default is to compile
          let unsorted_opts = if (null given_opts)
                                then [Compile]
                                else given_opts

          -- bring the action to the front by sorting
          let (action:opts) = sort $ unsorted_opts

          -- check for trivial actions, which do not need I/O file names
          case action of               
            Version         -> do putStrLn $ name ++ " " ++ Vers.getVersion
                                  exitWith ExitSuccess
            Help            -> do hPutStrLn stderr $ usage name
                                  exitWith ExitSuccess
            _               -> return ()
            

          -- need an input file
          infile    <- maybe (do hPutStrLn stderr $
                                        name ++ ": no input files.\nUse -h for help"
                                 exitFailure)
                             (return)
                             (listToMaybe args)

          -- get the input handle, a file or stdin
          hIn               <- openFH ReadMode infile

          -- get an output file name if an input was provided
          -- Maybe monad
          let outfile   = modFileExt infile $
                                   case action of
                                     Compile    -> "runtime"
                                     Run        -> "run"
                                     MakeGraph  -> "udg" -- UDrawGraph
                                     PruneGraph _ _ -> "pruned-cct"
                                     _        -> fail "" -- no output file in this case

          -- output handle:
          -- in order try: an '-o' option, input file with changed suffix.
          let fOut      = fromJust $ listToMaybe $ [f | Output f <- unsorted_opts]
                                                    ++ [outfile]
                                                     
          hOut          <- openFH WriteMode fOut

          case action of
            Compile         -> driveCompile infile hIn hOut opts
                               `trace` ("Compiling into " ++ fOut)
            Run             -> driveRunFile (getRTFlags opts) hIn hOut
            MakeGraph       -> driveGraph   hIn hOut
            PruneGraph s d  -> doPruneGraph s d hIn hOut

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
         doCompile 1 (getRTFlags opts) SrcPar.pProg fIn' hOut



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
    | RunFlag RunFlag           -- a run-time flag, defined in Common.hs

    | Output String
    deriving (Eq,Ord,Show)




-- possible extra files to generate during the compilation phase.
cCOMPILE_EXTRAS = [DumpGates, DumpGraph]
    

optionControl :: [Opt.OptDescr Flag]
optionControl = [
                 -- actions
      Opt.Option ['c']      ["compile"] (Opt.NoArg Compile)             "Compile SFDL (default action); .runtime"
    , Opt.Option ['G']      ["mkgraph"] (Opt.NoArg MakeGraph)           "generate a UDrawGraph file; .cct -> .udg"
    , Opt.Option ['r']      ["run"]     (Opt.NoArg Run)                 "Run circuit; .gates -> .run"
    , Opt.Option [   ]      ["prune"]   (Opt.ReqArg getPruneArgs
                                                    "<start node>,<path len>")
                                                                        "Prune the circuit around a given node;\n.cct -> .cct"

    , Opt.Option ['V','?']  ["version"] (Opt.NoArg Version)             "Show version number"
    , Opt.Option ['h']      ["help"]    (Opt.NoArg Help)                "Print help (this text)"

    , Opt.Option ['o']      ["output"]  (Opt.ReqArg Output "<file>")    "Output to <file>"

    , Opt.Option [  ]       []          (Opt.NoArg Help)                "Flags:"

                -- ############ flags

                -- here, internal form is using the standard (derived) Haskell Read/Show instances.
    , Opt.Option [  ]       ["dump-gates",
                             "dgt"]     (Opt.NoArg (RunFlag DumpGates)) "dump the list of gates in internal form; -> .gates"
    , Opt.Option [  ]       ["dump-graph",
                             "dgr"]     (Opt.NoArg (RunFlag DumpGraph)) "dump the graph in internal form; -> .cct"
    , Opt.Option ['p']      ["gen-print"
                            ]           (Opt.NoArg (RunFlag DoPrint))   "Generate Print gates from print() statements.\nIf not set, print statements are ignored."
    , Opt.Option ['v']      ["verbose"] (Opt.NoArg (RunFlag Verbose))   "Chatty output on stderr"

    , Opt.Option [  ]       ["bindump"] (Opt.NoArg (RunFlag RunTraceBinary)) "Produce a run trace in binary, for comparing with the real runtime"
     ]


getPruneArgs :: String -> Flag
getPruneArgs str = let (start,str1) = head $ reads str
                       (',':str2)   = str1
                       (dist,[])    = head $ reads str2
                   in
                     PruneGraph start dist

getRTFlags :: [Flag] -> [RunFlag]
getRTFlags flags = [rtf | (RunFlag rtf) <- flags]


usage name = Opt.usageInfo ("Usage: " ++ name ++
                            " <options> in.sfdl\n\
                            Compiles SFDL files, simulates a circuit, and manipulates circuit files.\n\
                            Options, with associated default output filename extensions:")
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

doCompile v rtFlags parser filenameIn hOut strIn =
    let tokens  = SrcPar.myLexer strIn
        ast     = parser tokens
    in case ast of
         SrcErrM.Bad s
                  -> do putStrLn "\nParse Failed...\n"
                        putStrV v "Tokens:"
                        putStrV v $ show tokens
                        putStrLn s
         SrcErrM.Ok  prog@(SrcAbs.Prog _ _) ->
           do logmsg INFO "Parse Successful!"
              let fix_prog = SrcFixup.fixupProg prog
              case typeCheck fix_prog of
                (Left err)        -> do hPutStrLn stderr $ "Type Error! " ++ showErrs err
                                        exitFailure
                (Right prog@(Im.Prog pname
                             Im.ProgTables {Im.types=typ_table,
                                            Im.funcs=fs}))      ->
                   do -- hPrint stderr prog
                      logmsg PROGRESS "Typechecking done"

#ifdef SYNTAX_C
       -- FIXME: the helper file generation shouldbe controlled by some options probably.
                      templ <- catch (openFile cC_HELPER_TEMPLATE ReadMode >>= hGetContents)
                                     (\e -> IOErr.ioError $
                                            if IOErr.isDoesNotExistError e
                                            then 
                                                 IOErr.annotateIOError
                                                 e
                                                 ("Could not find the helper template " ++ cC_HELPER_TEMPLATE ++
                                                  ", ensure it is present in the current dir.")
                                                 Nothing (Just cC_HELPER_TEMPLATE)
                                            else e)
                      let helper         = SrcGenHelper.genHelper templ prog
                          helperFileName = modFileExt filenameIn "helper.cc"
                      if not (null helper)
                        then writeFile helperFileName helper
                        else return ()   
#endif

                      let prog_flat = Ho.flattenProg prog
                      logmsg PROGRESS "Flattened program"
                      logmsg DEBUG $ "The flattened program:\n" ++
                                     strShow prog_flat
                      case Ur.unrollProg prog_flat of
                        (Left err)       -> do hPutStrLn stderr $ "Unrolling Error! " ++
                                                                  showErrs err
                                               exitFailure
                        (Right stms)     ->
                           do logmsg PROGRESS "Unrolled main; \
                                               Starting to generate the circuit"
                              logmsg DEBUG $ "The unrolled statements:\n" ++
                                                   PP.render (PP.vcat $ map Im.docStm stms)


                              let -- compile the circuit
                                  args          = CG.extractInputs prog
                                  (cct,gates)   = CG.genCircuit rtFlags typ_table stms args

                              -- write the .runtime file
                              mapM_ (hPutStr hOut . CG.cctShow) gates
                              logmsg PROGRESS "Wrote the circuit runtime file"

                              -- and the extra outputs requested
                              let extras        = rtFlags `intersect` cCOMPILE_EXTRAS
                              mapM_ (doExtras filenameIn (cct,gates)) extras


showErrs (EWC (e, cs))  = show e ++
                          concatMap (("\n\tIn context of:\n" ++)) (reverse cs)

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


doRunCct rtFlags gates_fh hOut = 
    do logmsg PROGRESS "Now running the circuit"
       gates    <- hGetContents gates_fh >>= readIO
       ins      <- Run.getUserInputs gates
       vals     <- Run.formatRun rtFlags gates ins
       mapM_ (hPutStrLn hOut) vals


-- | change a filename's extension (part after the last dot)
modFileExt file newext = let components = split '.' file
                         in (joinLists "." (init components)) ++ "." ++ newext


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
