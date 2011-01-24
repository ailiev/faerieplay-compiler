module Faerieplay.Version where

import IlievUtils.Misc          as SL

cIS_RELEASE = False

cRELEASE="0.8"

-- NOTE: this is updated by emacs function time-stamp; see emacs "Local Variables:"
-- section at the end.
-- g_timestamp = "2006-11-08 13:24:07 sasho"

-- these two updated by our build script, based on git metadata.
g_svn_id = "git c129fde32b6277e0a682f6c8eb4c7b48e456108c: Incorporate cabal into build. update hasktags."
g_svn_date = "2011-01-23 16:39:22 +0000"

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
</versions>
-}
