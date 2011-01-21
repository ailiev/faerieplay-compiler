module Faerieplay.Version where

import IlievUtils.Misc          as SL

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
/home/sasho/work/code/sfdl-compiler/Faerieplay/: 773:818M;
/home/sasho/work/code/sfdl-compiler/Faerieplay/Bnfc/Fcpp/: 773:814M;
/home/sasho/work/code/sfdl-compiler/Faerieplay/Bnfc/Sfdl/: 773:814;
Faerieplay/: 773:818M;
</versions>
-}
