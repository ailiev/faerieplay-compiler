module Intermediate where


import Text.PrettyPrint

-- import Tree

import List (intersperse)

import Control.Monad.Identity (runIdentity)

import qualified Data.Map as Map

import SashoLib (myLiftM, pop, push, modifyListHead)


data EntType = Type | Var deriving (Eq,Ord,Show)

-- Func takes:
-- - name
-- - return Typ
-- - list of formal params
-- - the list of statements
data Func = Func Ident Typ [Var] [Stm] deriving (Eq,Ord)

-- and the full name for an entity: its original name and an optional qualifier
type EntName = String

data VarFlag =
    LoopCounter
  | Immutable
  | FormalParam
  | Global
  | RetVar                      -- a return variable, ie. a var
                                -- representing its enclosing function
   deriving (Show,Ord,Eq)

type TypeTable = Map.Map EntName Typ

type FuncTable = Map.Map EntName Func




data ProgTables = ProgTables { types  :: TypeTable,
                               funcs  :: FuncTable }
    deriving (Show,Eq,Ord)




type Ident = String


data Prog =
   Prog Ident ProgTables
  deriving (Eq,Ord)



data Typ =
   IntT Exp
 | GenIntT
 | BoolT
 | VoidT
 | StructT [TypedName]          -- field names/types
 | EnumT String Int             -- name and bitsize. the identifiers are in the
                                -- global var table as instances of
                                -- (Enum name i)
 | ArrayT Typ Exp
 | SimpleT Ident
 | FuncT Typ [Typ]
 | RedIntT Integer              -- RedIntT =def IntT . ELit . LInt
 | RedArrayT Typ Integer        -- similar
  deriving (Eq,Ord)

data TypedName =
   TypedName Typ Ident
  deriving (Eq,Ord)

data Stm =
   SBlock [Stm]
 | SAss LVal Exp
 | SFor Ident Exp Exp Stm
 | SIf     Exp Stm
 | SIfElse Exp Stm Stm
  deriving (Eq,Ord)

data LVal =
   LVal Exp
  deriving (Eq,Ord)

data Exp =
   -- variables just need a name, and the scope stack will take care of
   -- making sure we get the correct instance of that name
   EVar Var
 | EStruct Exp String           -- Structure and field
 | EArr Exp Exp                 -- array
 | ELit Lit                     -- literal
 | EFunCall Ident [Exp]
 | UnOp UnOp Exp
 | BinOp BinOp Exp Exp
 | EGetBit Exp Exp              -- EGetBit i exp = i-th bit of exp
-- | EBitsize Exp                 -- bitsize of an int expression
 | EStatic Exp                  -- a static expression, computable at
                                -- compile time. use to mark all
                                -- static expressions
 | ExpT Typ Exp                 -- a type annotation
 | ESeq [Stm] Exp               -- several assignment statements,
                                -- followed by an expression.
  deriving (Eq,Ord)


-- some expressions have to be static, hence not annotated with EStatic:
-- arg of IntT


-- helpers for variables (without flags)
var = EVar . VSimple
tempVar = EVar . VTemp

data Var =
    VSimple Ident
  | VTemp Int
  | VFlagged [VarFlag] Var
  | VScoped Scope Var           -- during unrolling, everything ends
                                -- up in one scope; thus add a
                                -- list of the pre-unroll scope id's
    deriving (Eq,Ord)

-- CONVENTION: VScoped goes on the outside of VFlagged (as illustrated in
-- vflags), and they don't recurse further (that'd be a mess!)


-- quick helper to get the flags
vflags (VFlagged fl _)                  = fl
vflags (VScoped _ (VFlagged fl _))      = fl
vflags _                                = []


-- ScopeId, by example:
-- -       1
--   -     1,1
--   -     1,2
--     -   1,2,1
--     -   1,2,2
--   -     1,3
-- -       2
-- -       3

type Scope = [Int]


-- enter a new scope depth (eg. upon entering a function call)
-- uses the Stack class functions
-- pushScope :: Scope -> Scope
pushScope scope = push scope 0
popScope scope = pop scope

-- enter the next scope at the same depth (eg. from one function call to the
-- next)
-- for some reason, without the type signature the type of 1 was inferred as
-- Integer
-- incrScope :: Scope -> Scope
incrScope scope = modifyListHead (+1) scope


data Lit =                      -- literals
    LInt Int                    -- TODO: change to Integer
  | LBool Bool
   deriving (Eq,Ord)
             

data BinOp =
    Times 
  | Div 
  | Plus 
  | Minus 
  | SL 
  | SR 
  | Lt 
  | Gt 
  | LtEq 
  | GtEq 
  | Eq 
  | Neq 
  | BAnd 
  | BXor 
  | BOr 
  | And 
  | Or
--  and internal ones
  | Max
   deriving (Eq,Ord,Show)


data UnOp =
    Not
  | BNot
  | Neg
-- and internal:
  | Log2
  | Bitsize
   deriving (Eq,Show,Ord)


data Points a = P0 |
                P1   (a -> a)         a |
                P2   (a -> a -> a)   (a,a) |
                PList ([a] -> a)     [a]
-- etc...

classifyExp :: Exp -> (Points Exp)
classifyExp e =
    case e of
         (BinOp op e1 e2)  -> P2 (BinOp op) (e1,e2)
         (UnOp op e1)      -> P1 (UnOp op) e1
         (EStruct str fld) -> P1 (`EStruct` fld) str
         (EArr arr idx)    -> P2 EArr (arr , idx)
         (EGetBit base bit)-> P2 EGetBit (base,bit)
         (EStatic e)       -> P1 EStatic e
         (ExpT t e)        -> P1 (ExpT t) e
         (ESeq ss e1)      -> P1 (ESeq ss) e1
         (EFunCall nm args)-> PList (EFunCall nm) args
         _                 -> P0

-- returns the children statements and expressions of an Stm, and also
-- a constructor to construct the Stm from (new) children
stmChildren :: Stm -> ([Stm], [Exp], ([Stm] -> [Exp] -> Stm))
stmChildren s =
    case s of
      (SBlock stms)             -> ( stms,   [],         (\ss []        -> (SBlock ss)) )
      (SAss (LVal lval) val)    -> ( [],     [lval,val], (\[] [lval,val]-> (SAss (LVal lval) val)) )
      (SFor nm lo hi fors)      -> ( [fors], [lo,hi],    (\[s] [lo,hi]  -> (SFor nm lo hi s)) )
      (SIf test ifs)            -> ( [ifs] , [test],     (\[s] [test]   -> (SIf test s)) )


-- some recursive structure for Stm's and Exp's. Make it monadic for
-- generality
-- The children expressions of 'e' are processed first, and then 'f'
-- is called on the resulting Exp.
-- 'f' is called without recursion if there are no children Exp's (eg.
-- for EVar)
mapExpM :: (Monad m) => (Exp -> m Exp) -> Exp -> m Exp
mapExpM f e
    | P2 cons (e1,e2) <- eclass         = do e1_f <- mapExpM f e1
                                             e2_f <- mapExpM f e2
                                             f $ cons e1_f e2_f
    | P1 cons e1      <- eclass         = do e1_f <- mapExpM f e1
                                             f $ cons e1_f
    | PList cons es   <- eclass         = do es_f <- mapM (mapExpM f) es
                                             f $ cons es_f
    | P0              <- eclass         = f e
   where eclass = classifyExp e


-- use the Identity Monad to extract a non-monadic version of mapExpM
mapExp :: (Exp -> Exp) -> Exp -> Exp
mapExp f e = runIdentity (mapExpM (myLiftM f) e)



-- and a recursion structure for Stm's!
-- f_s: function on a statement
-- f_e: function on an expression (passed on to mapExpM)
mapStmM :: (Monad m) => (Stm -> m Stm) -> (Exp -> m Exp) -> Stm -> m Stm
mapStmM f_s f_e s = do let (ss, es, scons) = stmChildren s
                       new_es <- mapM (mapExpM f_e) es
                       new_ss <- mapM (mapStmM f_s f_e) ss
                       f_s (scons new_ss new_es)

mapStm :: (Stm -> Stm) -> (Exp -> Exp) -> Stm -> Stm
mapStm f_s f_e s = runIdentity (mapStmM (myLiftM f_s) (myLiftM f_e) s)

{-
-- generalize a bit to allow [Stm] to be produced
mapStmLM :: (Monad m) => (Stm -> m [Stm]) -> (Exp -> m Exp) -> Stm -> m [Stm]
mapStmLM f_s f_e s = do let (ss, es, scons) = stmChildren s
                        new_es  <- mapM (mapExpM  f_e) es
                        new_sss <- mapM (mapStmLM f_s f_e) ss
                        let new_ss = map ((flip scons) new_es) new_sss
                        mapM f_s new_ss
-}

{-
-- even more general
mapStmGenM :: (Monad m) => (Stm -> m a) -> (Exp -> m Exp) -> Stm -> m a
mapStmGenM f_s f_e s = do let (ss, es, scons) = stmChildren s
                          new_es <- mapM (mapExpM f_e) es
                          new_ss <- mapM (mapStmGenM f_s f_e) ss
                          f_s (scons (new_ss . conv) new_es)
-}



---------------------------
-- Tree instances
---------------------------
{-
instance Tree Exp where
    children e = [e]
    nodeExtr e     = 1
    nodeCons i es = 
-}


----------------------------
-- a pretty printer for this
----------------------------

docProg :: Prog -> Doc
docProg (Prog id (ProgTables {funcs=fs,
                              types=ts})) = let funcList   = Map.fold (:) [] fs
                                                -- typList is a list of (name , value):
                                                typList   = Map.foldWithKey collect [] ts
                                            in  vcat [text "Program" <+> text id <> colon,
                                                      text "",
                                                      text "Types:",
                                                      vcat (map (docPair docTyp) typList),
                                                      text "",
                                                      text "Functions:",
                                                      vcat (intersperse (text "") $ map docFunc funcList)]
    where collect key val accum = ((key,val) : accum)
          docPair sf (n,t)      = sep [text n, equals, sf t]
          docVar (t,flags)      = sep [docTyp t, parens (text $ show flags)]

docFunc (Func name t args stms) = vcat [text "function" <+> (text name) <+>
                                             (parens $ cat $ punctuate comma (map docVar args)),
                                        nest 2 (vcat (map docStm stms)),
                                        text "end",
                                        empty]

docStm :: Stm -> Doc
docStm (SAss (LVal lval) val)= sep [docExp lval, text "=", docExp val]
docStm (SBlock stms)         = vcat (map docStm stms)
                               
docStm (SFor nm hi lo stm)   = sep [text "for",
                                    parens $ sep [
                                                  text nm, text "=",
                                                  docExp hi, text "to", docExp lo
                                                 ]] $$
                               nest 4 (docStm stm)

docStm (SIf test stm)        = sep [text "if",
                                    parens $ docExp test] $$
                               nest 4 (docStm stm)

docStm (SIfElse test s1 s2)  = docStm (SIf test s1) $$
                               docStm s2


docExp e = case e of
    (EVar v)            -> parens $ docVar v
    (EStruct str field) -> cat [docExp str, text ".", text field]
    (EArr arr idx)      -> cat [docExp arr, text "[", docExp idx, text "]"]
    (ELit l)            -> docLit l
    (EFunCall f args)   -> cat [text f, text "(",
                                sep (punctuate (text ",") (map docExp args)),
                                text ")"]
    (UnOp op e)         -> cat [docUnOp op, text "(", docExp e, text ")"]
    (BinOp op e1 e2)    -> cat [docExp e1, docBinOp op, docExp e2]
    (EGetBit x b)       -> docExp (EArr x b)
    (EStatic e)         -> docExp e
    (ExpT t e)          -> docExp e
    (ESeq stms e)       -> brackets $
                           cat ((punctuate comma (map docStm stms))) <>
                           text ": " <>
                           docExp e


docTyp :: Typ -> Doc
docTyp t =
    case t of
      (IntT i)          -> cat [text "Int", text "<", docExp i, text "`>"]
      (GenIntT)         -> cat [text "Int", text "<`>"]
      (BoolT)           -> text "void"
      (VoidT)           -> text "void"
      (StructT fields)  -> sep [text "struct",
                                     braces $ sep $
                                     punctuate (text ",") $
                                     map docTypedName fields]
      (EnumT nm size)   -> cat [text "enum",
                                text "<", int size, text ">",
                                space, text nm]
      (ArrayT t size)   -> cat [docTyp t, brackets (docExp size)]
      (SimpleT name)    -> text name
      (FuncT t ts)      -> cat [parens $ cat $ punctuate (text ", ") (map docTyp ts),
                                text " -> ",
                                docTyp t]


docTypedName (TypedName t name) = sep [docTyp t, text name]

docVar v =
    case v of
      (VSimple name)    -> text name
      (VTemp i)         -> text "temp" <> (parens (int i))
      -- the scope list is reversed so we see the outermost scope (which is at
      -- the list end) first
      (VScoped scopes v)-> cat $ punctuate (text "/") (map int (reverse scopes)) ++ [text "/", docVar v]
      (VFlagged flags v)-> docVar v <> (text "\\") <> cat (map docVarFlag flags)


docVarFlag f = char (case f of
                           Global       -> 'g'
                           Immutable    -> 'i'
                           LoopCounter  -> 'l'
                           FormalParam  -> 'f'
                           RetVar       -> 'r')


docLit (LInt i)          = int i
docLit (LBool b)         = text $ show b


docUnOp o = case o of
                   Not  -> text "!"
                   BNot -> text "~"
                   Neg  -> text "-"
                   Log2 -> text "log2"
                   Bitsize -> text "bitsize"


docBinOp o = case o of
                    Times -> text "*" 
                    Div -> text "/" 
                    Plus -> text "+" 
                    Minus -> text "-" 
                    SL -> text "<<" 
                    SR -> text ">>" 
                    Lt -> text "<" 
                    Gt -> text ">" 
                    LtEq -> text "<=" 
                    GtEq -> text ">=" 
                    Eq -> text "==" 
                    Neq -> text "!=" 
                    BAnd -> text "&" 
                    BXor -> text "^" 
                    BOr -> text "|" 
                    And -> text "&&" 
                    Or -> text "||"


instance Show Stm where
    showsPrec i = showsPrec i . docStm

instance Show Exp where
    showsPrec i = showsPrec i . docExp

instance Show Typ where
    showsPrec i = showsPrec i . docTyp

instance Show Var where
    showsPrec i = showsPrec i . docVar

instance Show Prog where
    showsPrec i = showsPrec i . docProg

instance Show TypedName where
    showsPrec i = showsPrec i . docTypedName

instance Show Func where
    showsPrec i = showsPrec i . docFunc

instance Show Lit where
    showsPrec i = showsPrec i . docLit
