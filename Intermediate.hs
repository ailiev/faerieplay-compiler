module Intermediate where


import Text.PrettyPrint

-- import Tree


import qualified Data.Map as Map

data EntType = Type | Var deriving (Eq,Ord,Show)

data Func = Func Typ VarTable [Stm] deriving (Eq,Ord)

-- and the full name for an entity: its original name and an optional qualifier
type EntName = String

data VarFlag =
    LoopCounter
  | Immutable
  | RetVar                      -- a return variable, ie. a var
                                -- representing its enclosing function
   deriving (Show,Ord,Eq)

-- we'll have a stack of these, one per scope.
type VarTable = Map.Map EntName (Typ,[VarFlag])

type TypeTable = Map.Map EntName Typ

type FuncTable = Map.Map EntName Func

-- we go with integer consts for now
type ConstTable = Map.Map EntName Integer




data ProgTables = ProgTables { vars   :: [VarTable],
                               types  :: TypeTable,
                               consts :: ConstTable,
                               funcs  :: FuncTable    }
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


-- helpers for variables
var = EVar . VSimple
tempVar = EVar . VTemp

data Var =
    VSimple Ident
  | VTemp Int
  | VScoped Var [ScopeId]           -- during unrolling, everything ends
                                    -- up in one scope; thus add a
                                    -- list of the enclosing scope id's
    deriving (Eq,Ord)


type ScopeId = Int


data Lit =                      -- literals
    LInt Int
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
                P2   (a -> a -> a)   (a,a)
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
         _                 -> P0

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
                              types=ts,
                              consts=cs,
                              vars=vs})) = let funcList   = Map.fold (:) [] fs
                              -- list of (name , value):
                                               typList   = Map.foldWithKey collect [] ts
                                               constList  = Map.foldWithKey collect [] cs
                                               varList    = Map.foldWithKey collect [] (head vs)
                                            in  vcat [sep [text "Program", text id, colon],
                                                      text "\n",
                                                      text "Types:",
                                                      vcat (map (docPair docTyp) typList),
                                                      text "\n",
                                                      text "Variables:",
                                                      vcat (map (docPair docVar) varList),
                                                      text "\n",
                                                      text "Constants:",
                                                      vcat (map (docPair integer) constList),
                                                      text "\n",
                                                      sep [text "Functions", colon],
                                                      vcat (map docFunc funcList)]
    where collect key val accum = ((key,val) : accum)
          docPair sf (n,t)      = sep [text n, equals, sf t]
          docVar (t,flags)      = sep [docTyp t, parens (text $ show flags)]

docFunc (Func t vars stms)      = vcat [text "function:",
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
    (EVar v)            -> docVar v
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
