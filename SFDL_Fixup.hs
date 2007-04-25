{-# OPTIONS_GHC -fglasgow-exts #-}
-- -fglasgow-exts for Typeable

--
-- code to fix up the AST coming from the SFDL bnfc-generated parser.
-- Need to get rid of SizeExp nodes, and convert them to normal Exp.
--

module SFDL_Fixup where

import Data.Generics.Schemes    (everywhere)
import Data.Generics.Aliases    (mkT)

--import Data.Typeable            (Typeable)
--import Data.Generics            (Data)


import SFDL.Abs                 -- as Abs

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