{-# OPTIONS_GHC -fglasgow-exts #-}
-- -fglasgow-exts for Typeable

--
-- code to fix up the AST coming from the SFDL-C BNFC compiler.
--

module SFDL_C_Fixup where


import Data.Generics.Schemes    (everywhere, everything)
import Data.Generics.Aliases    (mkT, mkQ, extT)
import Data.Typeable            (Typeable)
import Data.Generics            (Data)

import SFDL_C.Abs     as CAbs
-- import qualified SFDL.Abs       as Abs


fixupProg :: CAbs.Prog -> CAbs.Prog
fixupProg = everywhere (mkT fixupDec .
                        mkT fixupTyp .
                        mkT fixupArr .
                        mkT fixupStm .
                        mkT fixupAss)


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
-- ASOpAss
fixupStm :: CAbs.Stm -> CAbs.Stm
fixupStm (SAssC ass)    =
    case ass of
             (ASOpAss   lval@(LVal lval_e) assop rval)  -> SAss lval ((assOp2Op assop) lval_e rval)
             _                                          -> error "Unexpected ASssC"
fixupStm stm            = stm


-- convert AssStm's to be all ASOpAss
fixupAss :: AssStm -> AssStm
fixupAss ass =
    case ass of
      (ASPostFix lval@(LVal lval_e) pfop)        -> let (op,arg) = pfOp2Op pfop
                                                    in  ASOpAss lval op arg 
      (ASPreFix  pfop lval@(LVal lval_e))        -> let (op,arg) = pfOp2Op pfop
                                                    in  ASOpAss lval op arg
      (ASOpAss _ _ _)                            -> ass
    


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

pfOp2Op pfop    = case pfop of PFIncr   -> (AssPlus, (EInt 1))
                               PFDecr   -> (AssMinus,  (EInt 1))


{-
convExp :: CAbs.Exp -> Abs.Exp
convExp CAbs.EInt = Abs.EInt
convExp CAbs.EPlus e1 e2 = Abs.EPlus (convExp e1) (convExp e2)
-}
