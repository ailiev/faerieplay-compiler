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

module Faerieplay.TestReads where

data D = D Int Char
       deriving (Show)

instance Read D where
    readsPrec _ s = do (i,s1)   <- reads s
                       (',',s2) <- reads s1
                       (c,s3)   <- reads s2
                       return $ (D i c, s3)
