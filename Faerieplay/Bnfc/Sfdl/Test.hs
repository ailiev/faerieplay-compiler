-- automatically generated by BNF Converter
module Main where


import IO ( stdin, hGetContents )
import System ( getArgs, getProgName )

import Faerieplay.Bnfc.Sfdl.Lex
import Faerieplay.Bnfc.Sfdl.Par
import Faerieplay.Bnfc.Sfdl.Skel
import Faerieplay.Bnfc.Sfdl.Print
import Faerieplay.Bnfc.Sfdl.Abs




import Faerieplay.Bnfc.Sfdl.ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree v tree



showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> hGetContents stdin >>= run 2 pProg
            "-s":fs -> mapM_ (runFile 0 pProg) fs
            fs -> mapM_ (runFile 2 pProg) fs




