{-# OPTIONS_GHC -fglasgow-exts #-}
-- -fglasgow-exts for Typeable

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


--
-- code to fix up the AST coming from the SFDL bnfc-generated parser.
-- Need to get rid of SizeExp nodes, and convert them to normal Exp.
--

module Faerieplay.SFDL_Fixup where

import Data.Generics.Schemes    (everywhere)
import Data.Generics.Aliases    (mkT)

--import Data.Typeable            (Typeable)
--import Data.Generics            (Data)


import Faerieplay.Bnfc.Sfdl.Abs                 -- as Abs

fixupProg :: Prog -> Prog
fixupProg = everywhere (mkT fixupIntT)

-- convert IntTConcrete nodes to IntT
fixupIntT :: Typ -> Typ
fixupIntT (IntTConcrete size_exp)   = IntT (se2e size_exp)
    where se2e se = case se of
                      SEIdent id    -> EIdent id
                      SEInt i       -> EInt i
                      SEFunCall id args
                          -> EFunCall id
                                      (map (\(SizeFunArg e) -> FunArg (se2e e)) args)
                      SEPlus x y    -> EPlus (se2e x) (se2e y)
                      SEMinus x y   -> EMinus (se2e x) (se2e y)
                      SETimes x y   -> ETimes (se2e x) (se2e y)
fixupIntT t                         = t
