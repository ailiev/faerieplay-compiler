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
/home/sasho/work/code/sfdl-compiler/Faerieplay/Version.hs.tok: /home/sasho/work/code/sfdl-compiler/Faerieplay/CircGen.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Common.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Container.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/ErrorWithContext.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/GenHelper_C.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/GenHelper_SFDL.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/GraphLib.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/HoistStms.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Intermediate.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Mapping.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Runtime.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/SashoLib.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/SFDL_C_Fixup.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/sfdlc.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/SFDL_Fixup.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Space-big.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Space.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Space-lean.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Stack.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/TestReads.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Tree.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/TreeLib.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/UDraw.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Unroll.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/TypeChecker.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Bnfc/Sfdl/Abs.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Bnfc/Sfdl/Skel.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Bnfc/Sfdl/Print.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Bnfc/Sfdl/Test.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Bnfc/Sfdl/ErrM.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Bnfc/Sfdl/Par.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Bnfc/Sfdl/Lex.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Bnfc/Fcpp/Abs.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Bnfc/Fcpp/Skel.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Bnfc/Fcpp/Print.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Bnfc/Fcpp/Test.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Bnfc/Fcpp/ErrM.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Bnfc/Fcpp/Par.hs: /home/sasho/work/code/sfdl-compiler/Faerieplay/Bnfc/Fcpp/Lex.hs: </versions>
-}
