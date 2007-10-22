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

module Faerieplay.Version where

import Faerieplay.SashoLib          as SL

cIS_RELEASE = False

cRELEASE="0.8"

-- NOTE: this is updated by emacs function time-stamp; see emacs "Local Variables:"
-- section at the end.
-- g_timestamp = "2006-11-08 13:24:07 sasho"

-- these two updated by subversion, with property "svn:keywords" set to at least
-- "Date Revision"
g_svn_id = "subversion  $Revision$"
g_svn_date = " $Date$"

svn_version = SL.trs [(" $",     ""),
                      ("$",      ""),
                      ("Revision:", "revision"),
                      ("Date: ", "")]
              $ g_svn_id ++ "; last modified on " ++ g_svn_date


getVersion
    | cIS_RELEASE   = cRELEASE
    | otherwise     = svn_version


-- the build scripts update the versions of every source file which is part of the project
-- here.
-- thus, this file will be seen as updated by the SCM if any of the other files are.
{-
<versions>
/home/sasho/work/code/sfdl-compiler/Faerieplay/: 664:668M;
/home/sasho/work/code/sfdl-compiler/Faerieplay/Bnfc/Fcpp/: 668;
/home/sasho/work/code/sfdl-compiler/Faerieplay/Bnfc/Sfdl/: 668;
</versions>
-}
