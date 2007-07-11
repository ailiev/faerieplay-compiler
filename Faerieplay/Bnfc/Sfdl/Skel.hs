module Faerieplay.Bnfc.Sfdl.Skel where

-- Haskell module generated by the BNF converter

import Faerieplay.Bnfc.Sfdl.Abs
import Faerieplay.Bnfc.Sfdl.ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x


transProg :: Prog -> Result
transProg x = case x of
  Prog id decs  -> failure x


transDec :: Dec -> Result
transDec x = case x of
  ConstDecl id exp  -> failure x
  TypeDecl id typ  -> failure x
  VarDecl typ ids  -> failure x
  FunDecl typ id typednames decs stms  -> failure x
  TypedFunDecl typ0 id typednames typ decs stms  -> failure x


transTyp :: Typ -> Result
transTyp x = case x of
  IntTConcrete sizeexp  -> failure x
  IntT exp  -> failure x
  GenIntT  -> failure x
  BoolT  -> failure x
  VoidT  -> failure x
  StructT typednames  -> failure x
  EnumT ids  -> failure x
  ArrayT typ exp  -> failure x
  RefT typ  -> failure x
  SimpleT id  -> failure x
  FuncT typ typs  -> failure x
  RedIntT n  -> failure x
  RedArrayT typ n  -> failure x


transTypedName :: TypedName -> Result
transTypedName x = case x of
  TypedName typ id  -> failure x


transStm :: Stm -> Result
transStm x = case x of
  SBlock decs stms  -> failure x
  SAss lval exp  -> failure x
  SPrint str exps  -> failure x
  SFor id exp0 exp stm  -> failure x
  SIf exp stm  -> failure x
  SIfElse exp stm0 stm  -> failure x


transLVal :: LVal -> Result
transLVal x = case x of
  LVal exp  -> failure x
  LValT typ lval  -> failure x


transExp :: Exp -> Result
transExp x = case x of
  EIdent id  -> failure x
  EInt n  -> failure x
  ETrue  -> failure x
  EFalse  -> failure x
  EArr exp0 exp  -> failure x
  EStruct exp0 exp  -> failure x
  EFunCall id funargs  -> failure x
  ENot exp  -> failure x
  ENeg exp  -> failure x
  EBNot exp  -> failure x
  ETimes exp0 exp  -> failure x
  EDiv exp0 exp  -> failure x
  EMod exp0 exp  -> failure x
  EPlus exp0 exp  -> failure x
  EMinus exp0 exp  -> failure x
  ESL exp0 exp  -> failure x
  ESR exp0 exp  -> failure x
  ELt exp0 exp  -> failure x
  EGt exp0 exp  -> failure x
  ELtEq exp0 exp  -> failure x
  EGtEq exp0 exp  -> failure x
  EEq exp0 exp  -> failure x
  ENeq exp0 exp  -> failure x
  EBAnd exp0 exp  -> failure x
  EBXor exp0 exp  -> failure x
  EBOr exp0 exp  -> failure x
  EAnd exp0 exp  -> failure x
  EOr exp0 exp  -> failure x
  ECoerce n exp  -> failure x
  EGetBit exp0 exp  -> failure x
  ExpT typ exp  -> failure x


transSizeExp :: SizeExp -> Result
transSizeExp x = case x of
  SEIdent id  -> failure x
  SEInt n  -> failure x
  SEFunCall id sizefunargs  -> failure x
  SETimes sizeexp0 sizeexp  -> failure x
  SEPlus sizeexp0 sizeexp  -> failure x
  SEMinus sizeexp0 sizeexp  -> failure x


transFunArg :: FunArg -> Result
transFunArg x = case x of
  FunArg exp  -> failure x


transSizeFunArg :: SizeFunArg -> Result
transSizeFunArg x = case x of
  SizeFunArg sizeexp  -> failure x



