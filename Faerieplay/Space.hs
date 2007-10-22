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


module Faerieplay.Main where

import SashoLib

-- repeat this many times at every recursion level
gREPCOUNT = 150
-- what to repeat
input = 3
-- how many recursion levels
gLEVELS = 3



unrollPair :: Int -> Int -> ([Int],Int)
-- base case at level 0: just return i, and a count of 1
unrollPair 0 i = ( [i], 1 )
-- replicate i and recurse on all the replicants, with (level-1).
-- Sum the counts of recursive calls
unrollPair level i = let is = replicate gREPCOUNT i
                         (iss,counts) = unzip $ map (unrollPair (level-1)) is
                     in (concat iss, sum counts)


main = do let (is,count) = unrollPair gLEVELS input
          print is
          putStrLn "Count:"
--          print count
