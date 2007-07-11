module Faerieplay.Main where

import Control.Monad.State
import Control.Monad.Writer

import Control.Monad.Error



-- This is the type of our type error representation.
data MyError = Err String deriving (Show)

-- We make it an instance of the Error class
instance Error MyError where
  noMsg    = Err "Anonymous error"
  strMsg s = Err s


type ErrMonad = Either MyError






-- repeat this many times
repCount = 160
input = 3
gLEVELS = 3


unroll :: Int -> Int -> [Int]
unroll 0 i              = [i]
unroll level i          = let is = replicate repCount i
                              iss = map (unroll (level-1)) is
                          in concat iss

minus :: (Num a) => a -> a -> a
minus = (-)


unrollSt :: Int -> Int -> State Int [Int]
unrollSt 0 i     = return [i]
unrollSt level i = do let is = replicate repCount i
                      iss <- mapM (unrollSt (level-1)) (map (`minus` 1) is)
                      modify (+ 1)
                      return (is ++ concat iss)


unrollErr :: Int -> Int -> Writer [MyError] [Int]
unrollErr 0 i     = return [i]
unrollErr level i = if i == 0
                    then do tell $ [Err "Cannot unroll 0!"]
                            return [i]
                    else do let is = replicate repCount i
                            iss <- mapM (unrollErr (level-1)) (map (`minus` 1) is)
                            return (is ++ concat iss)

unrollPair :: Int -> Int -> ([Int],Int)
-- base case at level 0: just return i, and a message
unrollPair 0 i = ( [i], 1 )
-- replicate i and recurse, with (level-1). No messages generated here
unrollPair level i = let is = replicate repCount i
                         (iss,counts) = unzip $ map (unrollPair (level-1)) is
                     in (concat iss, sum counts)
                    


getInputs :: IO [Int]
getInputs = do line <- getLine
               return $ map read (words line)


mainPlain = print (unroll gLEVELS input)

mainSt = let (out,st) = runState (unrollSt gLEVELS input) 0
         in  do print out
--                putStrLn "The state: " >> print st

mainErr = do let (is,errs) = runWriter $ unrollErr gLEVELS input
             print is
             putStrLn "Errors:"
             print errs

mainPair = do let (is,count) = unrollPair gLEVELS input
              print is
              putStrLn "Count:"
--              print count

--main = mainPlain
--main = mainSt
--main = mainErr
main = mainPair
