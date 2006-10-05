{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}
-- -fglasgow-exts: for "Non-type variables, or repeated type variables",
-- in evalStatic

module Intermediate where


import Text.PrettyPrint

-- import Tree

import List (intersperse)
import Maybe (fromJust)

import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Control.Monad.Error (Error, throwError, catchError, MonadError)
import Control.Monad.Identity (runIdentity)

import qualified Data.Map as Map

import SashoLib
import qualified Container as Cont

import Common (ErrMonad, MyError(..), MyErrorCtx, throwErrorCtx, trace)


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
data Func = Func Ident          -- func name
                 VarSet         -- local vars
                 Typ            -- return type
                 [TypedName]    -- arguments
                 [Stm]          -- and the function body
            deriving (Eq,Ord
                     , Show, Read
                     )


-- and the full name for an entity: its original name and an optional qualifier
type EntName = String

data VarFlag =
    LoopCounter
  | Immutable
  | FormalParam
  | Global
  | RetVar                      -- a return variable, ie. a var
                                -- representing its enclosing function
   deriving (Ord,Eq
            , Show, Read
            )


type TypeTable = Map.Map EntName Typ

class (Monad m) => TypeTableMonad m where
    getTypeTable :: m TypeTable


type FuncTable = Map.Map EntName Func


-- we'll have a stack of these, one per active scope, during
-- typechecking
type VarTable = Map.Map EntName (Typ,Var)


-- NOTE: Data.Map does not export a Read instance :(
data ProgTables = ProgTables { types  :: TypeTable,
                               funcs  :: FuncTable }
    deriving (Eq,Ord
             , Show
             )




type Ident = String


data Prog =
   Prog Ident ProgTables
  deriving (Eq,Ord
           , Show
           )


type TypedName = (Ident, Typ)

data Typ =
   IntT Exp
 | GenIntT
 | BoolT
 | VoidT
 | StructT ([TypedName],        -- fields' name and type
             [FieldLoc])        -- fields' locations
                    
 | EnumT String Int             -- name and bitsize. the identifiers are in the
                                -- global var table as instances of
                                -- (Enum name i)
 | ArrayT Typ Exp
 | SimpleT Ident
 | FuncT Typ [Typ]
 | RedIntT Int                  -- RedIntT =def IntT . ELit . LInt
 | RedArrayT Typ Int            -- similar

 | RefT Typ                     -- a reference type
  deriving (Eq,Ord
           ,Show,Read
           )

{-
data TypedName =
   TypedName Typ Ident
  deriving (Eq,Ord)
-}


-- I would ideally not have an SBlock here, but it conveys variable scoping
-- information, so we'll keep the blocks until scoping of variables (VScoped) is
-- done, then SBlock will be done away with
data Stm =
   SBlock VarSet [Stm]
 | SAss Exp Exp
 | SPrint String [Exp]
 | SFor Var Exp Exp [Stm]
 | SIfElse Exp (VarSet, [Stm]) (VarSet, [Stm])
  deriving (Eq,Ord
           , Show, Read
           )


-- an offset-length pair (in units of "primitive types", ie Int, Enum
-- and Bool) describing the location of a field in a struct
data FieldLoc = FieldLoc { byteloc :: (Int,Int), -- offset and length
                           valloc  :: (Int,Int) }
                deriving (Eq,Ord
                         , Show, Read
                         )

data FieldLocRecord = Byteloc | Valloc


data Exp =
   EVar Var
   -- Structure and field number (look up other infos in the StructT)
 | EStruct Exp Int
 | EArr Exp Exp                 -- array
 | ELit Lit                     -- literal
 | EFunCall Ident [Exp]
 | UnOp UnOp Exp
 | BinOp BinOp Exp Exp
 | EGetBit Exp Exp              -- EGetBit i exp = i-th bit of exp
 | EStatic Exp                  -- a static expression, computable at
                                -- compile time. use to mark all
                                -- static expressions
 | ExpT Typ Exp                 -- a type annotation
 | ESeq [Stm] Exp               -- several assignment statements,
                                -- followed by an expression.
   -- Indicate to the circuit generator (and runtime) that an
   -- array should be prepared
 | EArrayInit String            -- array name
              Int               -- element size
              Integer           -- array length
 | EStructInit Int              -- prepare a struct with that many primitive (Int) fields
                                -- and subfields
  deriving (Eq,Ord
           , Show, Read
           )




--
-- helpers for types
--

isRefType (RefT _)  = True
isRefType _         = False

-- remove the reference qualification on a type
stripRefQual (RefT t)   = t
stripRefQual t          = t

--
-- helpers for expressions
--

-- deep-remove all mentions of ExpT
stripExpT = mapExp f
    where f (ExpT t e)  = e
          f e           = e

-- get the type annotation of an expression. All should have one.
getExpTyp (ExpT t e) = t
getExpTyp e             = error $ "getExpTyp called on expression without type annotation: "
                                  << e


-- project a function on a fieldloc
proj_FL_val_off  f loc@(FieldLoc { valloc  = (off,len) })    = loc { valloc = (f off, len) }
proj_FL_byte_off f loc@(FieldLoc { byteloc = (off,len) })    = loc { byteloc = (f off, len) }






-- some expressions have to be static, hence not annotated with EStatic:
-- arg of IntT


--
-- helpers for variables
--
var = EVar . VSimple
tempVar = EVar . VTemp

-- get a type-annotated temp var, with the type of the given expression if it is in fact
-- annotated.
tempVarForExp exp = case exp of (ExpT t e)  -> ExpT t . tempVar
                                _           -> tempVar

varName (VSimple nm)    = nm
varName (VFlagged _ v)  = varName v
varName (VScoped _ v)   = varName v
varName (VTemp i)       = "temp_" ++ show i



-- variables just need a name, and the scope stack will take care of
-- making sure we get the correct instance of that name

-- CONVENTION: VScoped goes on the outside of VFlagged (as illustrated in
-- vflags), and they don't recurse further (that'd be a mess!)
data Var =
    VSimple Ident
  | VTemp Int
  | VFlagged [VarFlag] Var
  | VScoped Scope Var           -- during unrolling, everything ends
                                -- up in one scope; thus add a
                                -- list of the pre-unroll scope id's
    deriving (Eq,Ord
             , Show, Read
             )

-- | literals
data Lit =
    LInt Integer
  | LBool Bool
   deriving (Eq,Ord
            , Show, Read
            )
             
-- some helpers
lint = ELit . LInt
lbool = ELit . LBool


data BinOp =
    Times 
  | Div 
  | Mod
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
   deriving (Eq,Ord,Show,Read)


data UnOp =
    Not
  | BNot
  | Neg
-- and internal:
  | Log2
  | Bitsize
   deriving (Eq,Ord,Show,Read)



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





------------------
-- helpers for Typ
------------------


data ExpandTypFlags = DoFields  -- ^ recurse into Struct fields
                    | DoRefs    -- ^ recurse into the target type of a reference
                    | DoArrayElems -- ^ recurse into array element types
                    | DoTypeDefs  -- ^ recurse into type defs (SimpleT)
                    | DoAll     -- ^ recurse in all cases
    deriving (Eq,Show)


-- fully expand a type, recursing into complex types as specified by the flags
expandType :: (TypeTableMonad m) => [ExpandTypFlags] -> Typ -> m Typ
expandType flags t =
    do type_table <- getTypeTable
       return $ expandType' type_table flags t



expandType' :: TypeTable -> [ExpandTypFlags] -> Typ -> Typ
expandType' type_table flags t =
    let rec = expandType' type_table flags
    in
      case t of
                (SimpleT name)          -> worker DoTypeDefs
                                               (let t' = fromJustMsg
                                                           ("Intermediate.expandType " ++ name) $
                                                         Map.lookup name type_table
                                                in
                                                  rec t')
                (ArrayT elem_t
                           len)         -> worker DoArrayElems
                                               (ArrayT (rec elem_t) len)

                (StructT (name_typs,
                          locs))        -> worker DoFields
                                               (let (names,typs)    = unzip name_typs
                                                    typs'           = map rec typs
                                                in
                                                  StructT (zip names typs', locs))

                (RefT t)                -> worker DoRefs
                                                (RefT $ rec t)

                _                       -> t

          -- call a (recursive or terminal) function if 'flag' is set in the original
          -- 'flags' given to expandType, or if the DoAll flag is set. if not calling it,
          -- return the type given to expandType.
    where worker flag proc = if elem DoAll flags || elem flag flags
                             then proc
                             else t



-- for a structure, get a field name given the number
-- TODO: may need to expand the type in case of a type alias which points to a struct
-- type.
getFieldName (StructT (names,_))  fld_idx = fst $ names !! fld_idx
getFieldName t                    _       = "getFieldName called on non-struct type "
                                              ++ show t


-- | return the total (summed) length of a type,
-- which has already been expanded (ie. no SimpleT)
-- use 'tblen' for f to get the length of simple types: Int, Bool, EnumT
typeLength :: (Typ -> Int) -> Typ -> Int
typeLength f t =
    let rec = typeLength f in
    case t of
      (StructT (ts,
                locs))          -> (sum $
                                    map (rec . snd) $ -- snd pulls out the field's type
                                    ts)
                                     `trace`
                                     ("Intermediate.typeLength: got a StructT with fields "
                                      ++ (show . map fst) ts
                                      ++ "; and locs " ++ show locs)
                                   {- (sum $ map (snd . valloc) $ locs)
                                   `trace`
                                     ("Intermediate.typeLength: got a StructT with fields "
                                      ++ (show . map fst) ts
                                      ++ show locs) -}
                                   
      (SimpleT name)            -> 987654321
                                   `trace`
                                     ("ERROR: Intermediate.typeLength of SimpleT " << name)
                                      {-error $ "Intermediate: typeLength of a SimpleT "
                                           << name-}
      -- NOTE: dynamic arrays take up just one gate, hence always one
      -- doesnt make sense to ask for the binary size of a dynamic array, but need to
      -- rethink this
      -- FIXME: should return the size of the binary array pointer, 4 bytes?
--      (ArrayT t len)            -> 1
      t                         -> f t
      

-- extract some of the parameters of a StructT, given the whole parameter tuple
-- the location in words:
getStrTLocs     (_, locs)    = map valloc locs
-- the location in bytes:
getStrTByteLocs (_, locs)    = map byteloc locs


-- byte-lengths of primitive types
-- Added 1 byte to all the scalar byte sizes, used by the runtime to indicate if it's a
-- Just value or Nothing
tblen (IntT _)          = 4+1     -- NOTE: 32-bit integers!
tblen (BoolT)           = 1+1
tblen (EnumT _ bits)    = (bits `divUp` 8) + 1
tblen (ArrayT t len)    = cARRAY_BLEN

-- NOTE: array pointer byte size, including the Just indicator byte
-- This is used in other places too, so needs to be set to the full amount (ie. 5 not 4)
cARRAY_BLEN :: Int
cARRAY_BLEN = 4+1


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

add_vflags fl (VScoped sc (VFlagged fl' v)) = (VScoped sc (VFlagged (fl'++fl) v))
add_vflags fl (VScoped sc v)                = (VScoped sc (VFlagged (fl)      v))
add_vflags fl (VFlagged fl' v)              = (VFlagged (fl' ++ fl) v)
add_vflags fl v                             = (VFlagged fl v)

               

-- FIXME: strips the scope as well if there is one!
strip_vflags v = case stripScope v of
                                   (VFlagged _ v)       -> v
                                   _                    -> v

-- complete strip
strip_var = strip_vflags . stripScope





-----------------------------------
-- evaluation of static expressions
-----------------------------------

-- here, the error class is fixed, but the associated monad is generic, so could be a
-- State monad, or nothing
evalStatic :: (MonadError MyErrorCtx m) => Exp -> m Integer
evalStatic e = case e of
                      (BinOp op e1 e2)  -> do [i1,i2] <- mapM evalStatic [e1,e2]
                                              let realop = transIntOp op
                                              return $ i1 `realop` i2

                      (UnOp  op e1)     -> do i1 <- evalStatic e1
                                              let realop = transIntUnOp op
                                              return $ realop i1

                      (ELit l)          -> evalLit l
                      (ExpT _ e)        -> evalStatic e
                      (EStatic e)       -> evalStatic e
                      _                 -> throwErrorCtx $ Err 42 $ e << " is not static!"


-- try to evaluate an operation statically, fall back to an op on Exp if static fails
tryEvalStaticBin staticOp expOp x_e y_e = do [x, y] <- mapM evalStatic [x_e, y_e]
                                                          `catchError` const (return [-1, -1])
                                             return $ if x >= 0
                                                      then lint  $ staticOp x y
                                                      else BinOp expOp x_e y_e

-- a helper which just dies in case of error
evalStaticOrDie e = either (\e -> error ("evalStaticOrDie on " ++ show e ++
                                         "failed: " ++ show e))
                           (id)
                           (evalStatic e)


evalLit (LInt i)  = return i
evalLit (LBool b) = return $ toInteger $ fromEnum b

{-
-- evalStatic :: Exp -> ErrMonad Exp
evalStatic = mapExpM f
    where f e =
              case e of
                (BinOp op (ELit l1) (ELit l2))  -> evalBinOp op l1 l2
                (UnOp  op (ELit l1))            -> evalUnOp op l1
                (ELit l)                        -> e
                (EGetBit (ELit (LInt x))
                         (ELit (LInt i)))       -> fromIntegral $ testBit x i
                EStatic e                       -> e
                ExpT e                          -> e
                e                               -> throwErr 42 $ "Static expression " << e
                                                                 << " is not static"

          evalBinOp op (LInt i1)  (LInt i2)     = (transIntOp op) i1 i2
          evalBinOp op (LBool b1) (LBool b2)    = (transBoolOp op) b1 b2
          evalUnOp  op (LBool b1)               = (transBoolUnOp op) b1

-}

transIntOp op = case op of
                        Plus    -> (+)
                        Minus   -> (-)
                        Times   -> (*)
                        Div     -> div
                        BAnd    -> (.&.)
                        BOr     -> (.|.)
                        SL      -> \x s -> shiftL x (fromInteger s)
                        SR      -> \x s -> shiftR x (fromInteger s)
                        Max     -> max

-- FIXME: this is confused - these operators do return Bool, but not
-- all of them take only Bool
transBoolOp op = case op of
                         Or     -> (||)
                         And    -> (&&)
                         Eq     -> (==)
                         Gt     -> (>)
                         Lt     -> (<)
                         GtEq   -> (>=)

transBoolUnOp op = case op of
                           Not  -> not

transIntUnOp  op = case op of
                           BNot -> complement
                           Neg  -> negate
                           Log2 -> ilog2
                           Bitsize -> (max 1) . ilog2








-- on how many parameters does an expression work?
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
         (EStruct str fld)
                           -> P1 (`EStruct` fld) str
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
      (SPrint prompt vals)      -> ( [],     vals,       (\[] vs_new   -> (SPrint prompt vs_new)) )


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

-- SIfElse does not fit well into the simple stmChildren scheme, as it has two separate lists
-- of child statements
mapStmM f_s f_e (SIfElse test (locs1,stms1)
                              (locs2,stms2)) =
    do new_stms1 <- mapM (mapStmM f_s f_e) stms1
       new_stms2 <- mapM (mapStmM f_s f_e) stms2
       new_test  <- mapExpM f_e test
       f_s (SIfElse new_test (locs1,new_stms1) (locs2,new_stms2))

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

{-
  -- doesnt work, as <= has to return Bool and nothing else (like an expression which will
  -- evaluate to some bool later
instance Ord Exp where
    (ELit (LInt i1))    <=  (ELit (LInt i2))    = i1 <= i2
    e1                  <=  e2                  = BinOp $ LtEq e1 e2
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
                                                      vcat (intersperse (text "") $
                                                            map docFunc funcList)]
    where collect key val accum = ((key,val) : accum)
          docPair sf (n,t)      = sep [text n, equals, sf t]


docFunc (Func name vars t args stms) = vcat [text "function" <+> (text name) <+>
                                             (parens $ cat $
                                                       punctuate comma
                                                                 (map docTypedName args)),
                                             nest 2 (vcat (docVarSet vars :
                                                           text "-------------" :
                                                           (map docStm stms))),
                                             text "end",
                                             empty]

docStm :: Stm -> Doc
docStm (SBlock vars stms)   = nest 4 (vcat (docVarSet vars :
                                            text "-----------" :
                                            (map docStm stms)))
docStm (SAss lval val)      = sep [docExp lval, text "=", docExp val]
                               
docStm (SFor counter lo hi stms) = sep [text "for",
                                        parens $ sep [docVar counter, text "=",
                                                      docExp lo, text "to", docExp hi
                                                     ]] $$
                                   nest 4 (vcat (map docStm stms))

docStm (SIfElse test (_,s1s) (_,s2s)) = vcat [text "if",
                                              parens $ docExp test,
                                              nest 4 $ vcat (map docStm s1s),
                                              text "else",
                                              nest 4 $ vcat (map docStm s2s)]

docStm (SPrint prompt xs)               = cat [text "print",
                                               parens $ sep [ptext prompt,
                                                             comma,
                                                             sep $ punctuate comma
                                                                             (map docExp xs)]]


docExp e = case e of
    (EVar v)            -> docVar v

{-
    -- if we have a type annotation with a struct expression, use it to get the field name
    (ExpT t (EStruct str fld_idx))
                        -> let field_name = getFieldName t fld_idx
                           in  cat [docExp str, text ".", text field_name]
-}
    (EStruct str fld_idx)
                        -> cat [docExp str, text ".", int fld_idx]

    (EArr arr idx)      -> cat [docExp arr, text "[Arr:", docExp idx, text "]"]
    (ELit l)            -> docLit l
    (EFunCall f args)   -> cat [text f, text "(",
                                sep (punctuate (text ",") (map docExp args)),
                                text ")"]
    (UnOp op e)         -> cat [docUnOp op, text "(", docExp e, text ")"]
    (BinOp op e1 e2)    -> cat [docExp e1, docBinOp op, docExp e2]
    (EGetBit x b)       -> docExp (EArr x b)
    (EStatic e)         -> docExp e
    (ESeq stms e)       -> brackets $
                           cat ((punctuate comma (map docStm stms))) <>
                           text ": " <>
                           docExp e
    (EStructInit size) -> text "StructInit" <> parens (int size)
    (EArrayInit name elem_size len)  -> text "ArrayInit" <> text name <>
                                        parens (int elem_size <> comma <> integer len)
    -- throw away the expression type if not needed (ie. used above)
--    (ExpT t e)          -> docTyp t <> (parens $ docExp e)
    (ExpT t e)          -> docExp e

-- print a Typ for human eyes
docTyp :: Typ -> Doc
docTyp t =
    case t of
      (IntT size_e)     -> --let size = evalStaticOrDie size_e in
                           cat [text "Int", braces (docExp size_e)]
      (GenIntT)         -> cat [text "Int"]
      (BoolT)           -> text "bool"
      (VoidT)           -> text "void"
      (StructT (fields,
                _    )) -> sep [text "struct",
                                     braces $ sep $
                                     punctuate (text ",") $
                                     map docTypedName fields]
      (EnumT nm size)   -> cat [text "enum",
                                braces (int size),
                                space, text nm]
      (ArrayT t size_e) -> -- let size = evalStaticOrDie size_e in
                           cat [docTyp t, brackets (docExp size_e)]
      (SimpleT name)    -> text name
      (FuncT t ts)      -> cat [parens $ cat $ punctuate (text ", ") (map docTyp ts),
                                text " -> ",
                                docTyp t]
      (RefT t)          -> docTyp t <+> text "&"

-- print a Typ for the C++ runtime
docTypMachine t = 
    case t of
    -- an array is here specified by its length, and the byte size of its elements
      (ArrayT t size_e) -> let size     = evalStaticOrDie size_e
                               elt_size = typeLength tblen t
                                        {- case t of
                               (           StructT (_,locs  ))    -> sum $ map (snd . byteloc) $ locs
                                            _                       -> tblen t -}
                           in
                             sep [text "array", integer size, int elt_size]
      _                 -> text "scalar"


docTypedName (name,t) = sep [docTyp t, text name]

docVar v =
    case v of
      (VSimple name)    -> text name
      (VTemp i)         -> text "temp" <> parens (int i)
      -- the scope list is reversed so we see the outermost scope (which is at
      -- the list end) first
      (VScoped scopes v)-> parens $ cat $ punctuate (text "/") (map int (reverse scopes)) ++ [text "/", docVar v]
      (VFlagged flags v)-> parens $ docVar v <> (text "#") <> cat (map docVarFlag flags)


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
                    Mod     -> "%"
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
                    Or          -> "||"
                    Max         -> "`m`")

docFieldLoc (FieldLoc { valloc  = (voff,vlen),
                        byteloc = (boff,blen) })    = parens $ hcat [int voff,
                                                                     comma,
                                                                     int vlen]

instance StreamShow Prog where
    strShows = showsPrec 0 . docProg

instance StreamShow Stm where
    strShows = showsPrec 0 . docStm


instance StreamShow Exp where
    strShows = showsPrec 0 . docExp

instance StreamShow Typ where
    strShows = showsPrec 0 . docTyp

instance StreamShow Var where
    strShows = showsPrec 0 . docVar

instance StreamShow Func where
    strShows = showsPrec 0 . docFunc

instance StreamShow Lit where
    strShows = showsPrec 0 . docLit

instance StreamShow BinOp where
    strShows = showsPrec 0 . docBinOp

instance StreamShow UnOp where
    strShows = showsPrec 0 . docUnOp

instance StreamShow FieldLoc where
    strShows = showsPrec 0 . docFieldLoc
