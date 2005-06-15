module Intermediate where


import Text.PrettyPrint

-- import Tree

import List (intersperse)

import Control.Monad.Identity (runIdentity)

import qualified Data.Map as Map

import SashoLib (myLiftM, pop, push)
import qualified Container as Cont


data EntType = Type | Var deriving (Eq,Ord,Show)


-- We'll attach one of these to every scope-creating object
-- (which are Func and SBlock)
-- 
-- NOTE: VarSet is only accessed by Container class "methods", thus we
-- should only need to change this definition here (and recompile) to
-- use, for example, a (Data.Set Var) instead of a [Var]
type VarSet = [Var]


-- Func takes:
-- - name
-- - set of local variables in the function
-- - return Typ
-- - list of formal params
-- - the list of statements
data Func = Func Ident VarSet Typ [Var] [Stm]
            deriving (Eq,Ord)

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


-- we'll have a stack of these, one per active scope, during
-- typechecking
type VarTable = Map.Map EntName (Typ,Var)



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


-- I would ideally not have an SBlock here, but it conveys variable scoping
-- information, so we'll keep the blocks until scoping of variables (VScoped) is
-- done, then SBlock will be done away with
data Stm =
   SBlock VarSet [Stm]
 | SAss Exp Exp
 | SFor Var Exp Exp [Stm]
 | SIf     Exp [Stm]
 | SIfElse Exp [Stm] [Stm]
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

data Lit =                      -- literals
    LInt Integer                -- TODO: change to Integer
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
   deriving (Eq,Ord)


data UnOp =
    Not
  | BNot
  | Neg
-- and internal:
  | Log2
  | Bitsize
   deriving (Eq,Ord)



-----------------------
-- Var scopes
-----------------------

-- a new scope depth is generated (during unrolling) by a statement block,
-- for-loop body, and function body
--
-- every consecutive scope-creating object at the same depth uses an
-- incremented value for its scope depth
--
-- example (where a dash is a scope-creating object):
-- -       1
--   -     1,1
--   -     1,2
--     -   1,2,1
--     -   1,2,2
--   -     1,3
-- -       2
-- -       3

type Scope = [Int]






------------------------
-- helpers for Var
------------------------

-- quick helper to get the flags
vflags (VFlagged fl _)                  = fl
vflags (VScoped _ (VFlagged fl _))      = fl
vflags _                                = []

-- remove the scope from a Var
stripScope (VScoped sc v)             = v
stripScope v                          = v

-- if already scoped, just overwrite the old scope
addScope sc (VScoped scope_old v)     = (VScoped sc v)
addScope sc v                         = (VScoped sc v)
               

strip_vflags v = case stripScope v of
                                   (VFlagged _ v)       -> v
                                   v                    -> v





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
      (SBlock vars ss)          -> ( ss,     [],         (\ss []        -> (SBlock vars ss)) )
      (SAss lval val)           -> ( [],     [lval,val], (\[] [lval,val]-> (SAss lval val)) )
      (SFor nm lo hi fors)      -> ( fors,   [lo,hi],    (\ss  [lo,hi]  -> (SFor nm lo hi ss)) )
      (SIf test ifs)            -> ( ifs ,   [test],     (\ss  [test]   -> (SIf test ss)) )


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



instance Num Exp where
    -- some specific cases, which can be simplified
    (ELit (LInt i1)) + (ELit (LInt i2)) = (ELit $ LInt $ i1 + i2)
    e1 + e2 = BinOp Plus e1 e2

    (ELit (LInt i1)) * (ELit (LInt i2)) = (ELit $ LInt $ i1 * i2)
    e1 * e2                             = BinOp Times e1 e2

    (ELit (LInt i1)) - (ELit (LInt i2)) = (ELit $ LInt $ i1 - i2)
    e1 - e2 = BinOp Minus e1 e2 

    negate (ELit (LInt i))              = (ELit $ LInt (-1))
    negate e                            = UnOp Neg e

    abs      = id
    signum   = id
    fromInteger i = ELit $ LInt $ fromInteger i



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

docFunc (Func name vars t args stms) = vcat [text "function" <+> (text name) <+>
                                             (parens $ cat $ punctuate comma (map docVar args)),
                                             nest 2 (vcat (docVarSet vars :
                                                           text "-------------" :
                                                           (map docStm stms))),
                                             text "end",
                                             empty]

docStm :: Stm -> Doc
docStm (SBlock vars stms)    = nest 4 (vcat (docVarSet vars :
                                             text "-----------" :
                                             (map docStm stms)))
docStm (SAss lval val)= sep [docExp lval, text "=", docExp val]
                               
docStm (SFor counter lo hi stms) = sep [text "for",
                                        parens $ sep [docVar counter, text "=",
                                                      docExp lo, text "to", docExp hi
                                                     ]] $$
                                   nest 4 (vcat (map docStm stms))

docStm (SIf test stms)       = sep [text "if",
                                    parens $ docExp test] $$
                               nest 4 (vcat (map docStm stms))

docStm (SIfElse test s1s s2s)= vcat [docStm (SIf test s1s),
                                     text "else",
                                     nest 4 (vcat (map docStm s2s))]


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

docVarSet :: VarSet -> Doc
docVarSet m = vcat $ map docVar (Cont.toList m)



docLit (LInt i)          = integer i
docLit (LBool b)         = text $ show b


docUnOp o = text (case o of
                   Not  -> "!"
                   BNot -> "~"
                   Neg  -> "-"
                   Log2 -> "log2"
                   Bitsize -> "bitsize")


docBinOp o = text (case o of
                    Times -> "*" 
                    Div -> "/" 
                    Plus -> "+" 
                    Minus -> "-" 
                    SL -> "<<" 
                    SR -> ">>" 
                    Lt -> "<" 
                    Gt -> ">" 
                    LtEq -> "<=" 
                    GtEq -> ">=" 
                    Eq -> "==" 
                    Neq -> "!=" 
                    BAnd -> "&" 
                    BXor -> "^" 
                    BOr -> "|" 
                    And -> "&&" 
                    Or -> "||")


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

instance Show BinOp where
    showsPrec i = showsPrec i . docBinOp

instance Show UnOp where
    showsPrec i = showsPrec i . docUnOp