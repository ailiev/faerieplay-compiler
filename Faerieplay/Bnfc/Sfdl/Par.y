-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Faerieplay.Bnfc.Sfdl.Par where
import Faerieplay.Bnfc.Sfdl.Abs
import Faerieplay.Bnfc.Sfdl.Lex
import Faerieplay.Bnfc.Sfdl.ErrM
}

%name pProg Prog
%name pStm Stm
%name pExp Exp

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token 
 '{' { PT _ (TS "{") }
 '}' { PT _ (TS "}") }
 ',' { PT _ (TS ",") }
 '=' { PT _ (TS "=") }
 ';' { PT _ (TS ";") }
 '(' { PT _ (TS "(") }
 ')' { PT _ (TS ")") }
 ':' { PT _ (TS ":") }
 '<' { PT _ (TS "<") }
 '>' { PT _ (TS ">") }
 '<`>' { PT _ (TS "<`>") }
 '[' { PT _ (TS "[") }
 ']' { PT _ (TS "]") }
 '&' { PT _ (TS "&") }
 '.' { PT _ (TS ".") }
 '!' { PT _ (TS "!") }
 '-' { PT _ (TS "-") }
 '~' { PT _ (TS "~") }
 '*' { PT _ (TS "*") }
 '/' { PT _ (TS "/") }
 '%' { PT _ (TS "%") }
 '+' { PT _ (TS "+") }
 '<<' { PT _ (TS "<<") }
 '>>' { PT _ (TS ">>") }
 '<=' { PT _ (TS "<=") }
 '>=' { PT _ (TS ">=") }
 '==' { PT _ (TS "==") }
 '!=' { PT _ (TS "!=") }
 '^' { PT _ (TS "^") }
 '|' { PT _ (TS "|") }
 '&&' { PT _ (TS "&&") }
 '||' { PT _ (TS "||") }
 'Boolean' { PT _ (TS "Boolean") }
 'Int' { PT _ (TS "Int") }
 'cast' { PT _ (TS "cast") }
 'const' { PT _ (TS "const") }
 'else' { PT _ (TS "else") }
 'enum' { PT _ (TS "enum") }
 'false' { PT _ (TS "false") }
 'for' { PT _ (TS "for") }
 'function' { PT _ (TS "function") }
 'if' { PT _ (TS "if") }
 'print' { PT _ (TS "print") }
 'program' { PT _ (TS "program") }
 'struct' { PT _ (TS "struct") }
 'to' { PT _ (TS "to") }
 'true' { PT _ (TS "true") }
 'type' { PT _ (TS "type") }
 'var' { PT _ (TS "var") }
 'void' { PT _ (TS "void") }

L_ident  { PT _ (TV $$) }
L_quoted { PT _ (TL $$) }
L_integ  { PT _ (TI $$) }
L_err    { _ }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
String  :: { String }  : L_quoted { $1 }
Integer :: { Integer } : L_integ  { (read $1) :: Integer }

Prog :: { Prog }
Prog : 'program' Ident '{' ListDec '}' { Prog $2 (reverse $4) } 


ListDec :: { [Dec] }
ListDec : {- empty -} { [] } 
  | ListDec Dec { flip (:) $1 $2 }


ListStm :: { [Stm] }
ListStm : {- empty -} { [] } 
  | ListStm Stm { flip (:) $1 $2 }


ListIdent :: { [Ident] }
ListIdent : Ident { (:[]) $1 } 
  | Ident ',' ListIdent { (:) $1 $3 }


Dec :: { Dec }
Dec : 'const' Ident '=' Exp ';' { ConstDecl $2 $4 } 
  | 'type' Ident '=' Typ ';' { TypeDecl $2 $4 }
  | 'var' Typ ListIdent ';' { VarDecl $2 $3 }
  | 'function' Typ Ident '(' ListTypedName ')' '{' ListDec ListStm '}' { FunDecl $2 $3 $5 (reverse $8) (reverse $9) }
  | 'function' Typ Ident '(' ListTypedName ')' ':' Typ '{' ListDec ListStm '}' { TypedFunDecl $2 $3 $5 $8 (reverse $10) (reverse $11) }


Typ :: { Typ }
Typ : 'Int' '<' SizeExp '>' { IntTConcrete $3 } 
  | 'Int' '<`>' { GenIntT }
  | 'Boolean' { BoolT }
  | 'void' { VoidT }
  | 'struct' '{' ListTypedName '}' { StructT $3 }
  | 'enum' '{' ListIdent '}' { EnumT $3 }
  | Typ '[' Exp ']' { ArrayT $1 $3 }
  | Typ '&' { RefT $1 }
  | Ident { SimpleT $1 }


TypedName :: { TypedName }
TypedName : Typ Ident { TypedName $1 $2 } 


ListTypedName :: { [TypedName] }
ListTypedName : {- empty -} { [] } 
  | TypedName { (:[]) $1 }
  | TypedName ',' ListTypedName { (:) $1 $3 }


Stm :: { Stm }
Stm : '{' ListDec ListStm '}' { SBlock (reverse $2) (reverse $3) } 
  | LVal '=' Exp ';' { SAss $1 $3 }
  | 'print' '(' String ',' ListExp ')' { SPrint $3 $5 }
  | 'for' '(' Ident '=' Exp 'to' Exp ')' Stm { SFor $3 $5 $7 $9 }
  | 'if' '(' Exp ')' Stm { SIf $3 $5 }
  | 'if' '(' Exp ')' Stm 'else' Stm { SIfElse $3 $5 $7 }
  | Stm ';' { $1 }


ListExp :: { [Exp] }
ListExp : {- empty -} { [] } 
  | Exp { (:[]) $1 }
  | Exp ',' ListExp { (:) $1 $3 }


LVal :: { LVal }
LVal : Exp { LVal $1 } 


Exp13 :: { Exp }
Exp13 : Ident { EIdent $1 } 
  | Integer { EInt $1 }
  | 'true' { ETrue }
  | 'false' { EFalse }
  | '(' Exp ')' { $2 }


Exp12 :: { Exp }
Exp12 : Exp12 '[' Exp ']' { EArr $1 $3 } 
  | Exp12 '.' Exp13 { EStruct $1 $3 }
  | Ident '(' ListFunArg ')' { EFunCall $1 $3 }
  | Exp13 { $1 }


Exp11 :: { Exp }
Exp11 : '!' Exp12 { ENot $2 } 
  | '-' Exp12 { ENeg $2 }
  | '~' Exp12 { EBNot $2 }
  | Exp12 { $1 }


Exp10 :: { Exp }
Exp10 : Exp10 '*' Exp11 { ETimes $1 $3 } 
  | Exp10 '/' Exp11 { EDiv $1 $3 }
  | Exp10 '%' Exp11 { EMod $1 $3 }
  | Exp11 { $1 }


Exp9 :: { Exp }
Exp9 : Exp9 '+' Exp10 { EPlus $1 $3 } 
  | Exp9 '-' Exp10 { EMinus $1 $3 }
  | Exp10 { $1 }


Exp8 :: { Exp }
Exp8 : Exp9 '<<' Exp9 { ESL $1 $3 } 
  | Exp9 '>>' Exp9 { ESR $1 $3 }
  | Exp9 { $1 }


Exp7 :: { Exp }
Exp7 : Exp8 '<' Exp8 { ELt $1 $3 } 
  | Exp8 '>' Exp8 { EGt $1 $3 }
  | Exp8 '<=' Exp8 { ELtEq $1 $3 }
  | Exp8 '>=' Exp8 { EGtEq $1 $3 }
  | Exp8 { $1 }


Exp6 :: { Exp }
Exp6 : Exp7 '==' Exp7 { EEq $1 $3 } 
  | Exp7 '!=' Exp7 { ENeq $1 $3 }
  | Exp7 { $1 }


Exp5 :: { Exp }
Exp5 : Exp5 '&' Exp6 { EBAnd $1 $3 } 
  | Exp6 { $1 }


Exp4 :: { Exp }
Exp4 : Exp4 '^' Exp5 { EBXor $1 $3 } 
  | Exp5 { $1 }


Exp3 :: { Exp }
Exp3 : Exp3 '|' Exp4 { EBOr $1 $3 } 
  | Exp4 { $1 }


Exp2 :: { Exp }
Exp2 : Exp2 '&&' Exp3 { EAnd $1 $3 } 
  | Exp3 { $1 }


Exp1 :: { Exp }
Exp1 : Exp1 '||' Exp2 { EOr $1 $3 } 
  | Exp2 { $1 }


Exp :: { Exp }
Exp : Exp1 { $1 } 


SizeExp3 :: { SizeExp }
SizeExp3 : Ident { SEIdent $1 } 
  | Integer { SEInt $1 }
  | Ident '(' ListSizeFunArg ')' { SEFunCall $1 $3 }
  | '(' SizeExp ')' { $2 }


SizeExp2 :: { SizeExp }
SizeExp2 : SizeExp2 '*' SizeExp3 { SETimes $1 $3 } 
  | SizeExp3 { $1 }


SizeExp1 :: { SizeExp }
SizeExp1 : SizeExp1 '+' SizeExp2 { SEPlus $1 $3 } 
  | SizeExp1 '-' SizeExp2 { SEMinus $1 $3 }
  | SizeExp2 { $1 }


SizeExp :: { SizeExp }
SizeExp : SizeExp1 { $1 } 


FunArg :: { FunArg }
FunArg : Exp { FunArg $1 } 


ListFunArg :: { [FunArg] }
ListFunArg : {- empty -} { [] } 
  | FunArg { (:[]) $1 }
  | FunArg ',' ListFunArg { (:) $1 $3 }


SizeFunArg :: { SizeFunArg }
SizeFunArg : SizeExp { SizeFunArg $1 } 


ListSizeFunArg :: { [SizeFunArg] }
ListSizeFunArg : {- empty -} { [] } 
  | SizeFunArg { (:[]) $1 }
  | SizeFunArg ',' ListSizeFunArg { (:) $1 $3 }


ListTyp :: { [Typ] }
ListTyp : {- empty -} { [] } 
  | ListTyp Typ { flip (:) $1 $2 }



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map prToken (take 4 ts))

myLexer = tokens
}
