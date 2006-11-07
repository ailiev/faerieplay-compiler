{-# OPTIONS_GHC -fglasgow-exts #-}
-- -fglasgow-exts for Typeable

--
-- code to fix up the AST coming from the SFDL-C BNFC compiler.
--

module SFDL_C_Fixup where


import Data.Generics.Schemes    (everywhere, everything)
import Data.Generics.Aliases    (mkT, mkQ)
import Data.Typeable            (Typeable)
import Data.Generics            (Data)

import SFDL_C.Abs     as CAbs
-- import qualified SFDL.Abs       as Abs


fixupProg :: CAbs.Prog -> CAbs.Prog
fixupProg = everywhere (mkT fixupArr . mkT fixupAssStm)


-- convert the ArrVarDecl to the normal VarDecl
fixupArr :: CAbs.Dec -> CAbs.Dec
fixupArr (ArrVarDecl t name len)    = VarDecl (ArrayT t len) [name]
fixupArr dec                        = dec

-- convert all the funky (SAssC AssStm ...) nodes to SAss
fixupAssStm :: CAbs.Stm -> CAbs.Stm
fixupAssStm (SAssC ass) =
    case ass of
             (ASOpAss   lval@(LVal lval_e) assop rval)  -> SAss lval ((assOp2Op assop) lval_e rval)
             (ASPostFix lval@(LVal lval_e) pfop)        -> SAss lval ((pfOp2Op pfop)   lval_e)
             (ASPreFix  pfop lval@(LVal lval_e))        -> SAss lval ((pfOp2Op pfop)   lval_e)
fixupAssStm stm         = stm

-- SForC can't really be done at this point, should add it to the intermediate format,
-- handle it in TypeChecker.hs etc, and
-- use it in Unroll.hs


-- get a binary operator corresponding to an assignment operator.
assOp2Op assop  = case assop of AssId    -> (\_ rval -> rval)
                                AssPlus  -> EPlus
                                AssMinus -> EMinus
                                AssTimes -> ETimes
                                AssDiv   -> EDiv
                                AssMod   -> EMod

pfOp2Op pfop    = case pfop of PFIncr   -> (\e -> EPlus  e (EInt 1))
                               PFDecr   -> (\e -> EMinus e (EInt 1))
