module Faerieplay.Main where


-- repeat this many times
repCount = 70
-- what to repeat
input = 3
-- how many recursion levels
gLEVELS = 3



unrollPair :: Int -> Int -> ([Int],Int)
-- base case at level 0: just return i, and a count of 1
unrollPair 0 i = ( [i], 1 )
-- replicate i and recurse, with (level-1). Sum the counts of recursive calls
unrollPair level i = let is = replicate repCount i
                         (iss,counts) = unzip $ map (unrollPair (level-1)) is
                     in (concat iss, sum counts)


main = do let (is,count) = unrollPair gLEVELS input
          print is
          putStrLn "Count:"
          print count
