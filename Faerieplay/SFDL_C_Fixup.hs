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
-- code to fix up the AST coming from the Fcpp BNFC compiler.
--

module Faerieplay.SFDL_C_Fixup where


import Data.Generics.Schemes    (everywhere, everything)
import Data.Generics.Aliases    (mkT, mkQ, extT)
import Data.Typeable            (Typeable)
import Data.Generics            (Data)

import Faerieplay.Bnfc.Fcpp.Abs     as CAbs



fixupProg :: CAbs.Prog -> CAbs.Prog
fixupProg = everywhere (mkT fixupDec
                        . mkT fixupTyp
                        . mkT fixupArr
                        . mkT fixupStm
                        . mkT fixupAss)


fixupDec :: CAbs.Dec -> CAbs.Dec
fixupDec (TypeDecl_C typ name)      = TypeDecl name typ
fixupDec dec                        = dec

fixupTyp :: CAbs.Typ -> CAbs.Typ
fixupTyp (IntT_C)                   = IntT (EInt 32)
fixupTyp typ                        = typ


-- convert the ArrVarDecl to the normal VarDecl
fixupArr :: CAbs.Dec -> CAbs.Dec
fixupArr (ArrVarDecl t name len)    = VarDecl (ArrayT t len) [name]
fixupArr dec                        = dec


-- convert all the funky (SAssC AssStm ...) nodes to SAss
-- The enclosed AssStm's should have been cleaned up by fixupAss by now, and be just
-- ASOpAss with AssId
fixupStm :: CAbs.Stm -> CAbs.Stm
fixupStm (SAssC ass)    =
    case ass of
             (ASOpAss   lval AssId rval)        -> SAss lval rval
             _                                  -> error "Unexpected ASssC"
fixupStm stm            = stm


-- convert AssStm's to be all ASOpAss (ie leave just "x `op`= y", and no post/pre-fix
-- assignments)
-- Also, use just the AssId assign-operator, and put any actual operator inside the RHS,
-- eg "x += y" -> "x = x + y"
-- At the end, we just have assignments of the form "x = x `op` y"
fixupAss :: AssStm -> AssStm
fixupAss ass =
    case ass of
      (ASPostFix lval@(LVal lval_e) pfop)        -> genAss lval pfop
      (ASPreFix  pfop lval@(LVal lval_e))        -> genAss lval pfop
      (ASOpAss lval@(LVal lval_e) op rval)       -> let rval'   = (assOp2Op op) lval_e rval
                                                    in  ASOpAss lval AssId rval'

    where genAss lval@(LVal lval_e) pfop = let (op,arg) = pfOp2Op pfop
                                               rval     = (assOp2Op op) lval_e arg
                                           in  ASOpAss lval AssId rval
    


-- SForC can't really be done at this point, handle it in TypeChecker.hs etc.


-- get a binary operator corresponding to an assignment operator.
assOp2Op assop  = case assop of AssId    -> (\_ rval -> rval)
                                AssPlus  -> EPlus
                                AssMinus -> EMinus
                                AssTimes -> ETimes
                                AssDiv   -> EDiv
                                AssMod   -> EMod

pfOp2Op pfop    = case pfop of PFIncr   -> (AssPlus, (EInt 1))
                               PFDecr   -> (AssMinus,  (EInt 1))


{-
convExp :: CAbs.Exp -> Abs.Exp
convExp CAbs.EInt = Abs.EInt
convExp CAbs.EPlus e1 e2 = Abs.EPlus (convExp e1) (convExp e2)
-}
