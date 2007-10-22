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


module Faerieplay.GenHelper_SFDL where

import qualified Faerieplay.Intermediate        as Im

-- do not need a helper for SFDL
genHelper :: String -> Im.Prog -> String
genHelper s p = ""
