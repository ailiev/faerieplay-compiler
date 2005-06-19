-- code to split the AST into several namespaces of global bindings:
-- constants, types, variables and functions

module TypeChecker where

import Monad    (foldM, msum, liftM, zipWithM)
import List     (find, findIndex)
import Maybe    (fromJust)

import Debug.Trace      (trace)

import qualified Data.Map as Map
import qualified Control.Monad.State as St --- (MonadState, State, StateT, modify, runStateT)
import Control.Monad.Writer (Writer, runWriter, tell)
import Control.Monad.Error (Error, throwError, catchError, noMsg, strMsg)
import Control.Monad.Trans (lift)

import Common (MyError(..), ErrMonad)

import SashoLib (Stack(..), (<<), ilog2, maybeLookup,
                 myLiftM, concatMapM)
import qualified Container as Cont


-- the abstract syntax tree description
import qualified SFDL.Abs as T
import qualified SFDL.Print as Print

import qualified Intermediate as Im


type TypeError = MyError



data BinOpType = Arith | Logical | Binary | Comparison | NotBin deriving (Eq,Show)


-- data EntType = Var | Func | Type | Const deriving (Eq,Show)

-- we'll have a stack of these, one per active scope, during
-- typechecking
-- type VarTable = Map.Map Im.EntName (Im.Typ,Im.Var)

-- we go with integer consts for now
type ConstTable = Map.Map Im.EntName Integer


data TCState = TCS { vars   :: [Im.VarTable],
                     types  :: Im.TypeTable,
                     consts :: ConstTable,
                     funcs  :: Im.FuncTable    }
    deriving (Show,Eq,Ord)



type MyStateT = TCState


type StateWithErr = St.StateT MyStateT ErrMonad


throwErr :: Int -> String -> StateWithErr a
throwErr p msg = lift $ Left $ Err p msg






typeCheck :: T.Prog -> ErrMonad Im.Prog
typeCheck p = let startState = TCS { vars   = [Map.empty],
                                               consts = Map.empty,
                                               funcs  = Map.empty,
                                               types  = Map.empty  }
                  out = St.runStateT (checkProg p) startState
              in  case out of
                  Left err           -> Left err
                  Right (p_new, _ )  -> Right p_new





-- take a function which takes and returns the ConstTable, and project it onto
-- the whole MyStateT, keeping the other fields fixed
-- projFromConst :: (ConstTable -> ConstTable) -> MyStateT -> MyStateT
projFromConst  f ms@(TCS {consts = ct})   = ms { consts = f ct }

projFromTypes f ms@(TCS {types = ts})    = ms { types = f ts }

projFromFuncs f ms@(TCS {funcs = fs})    = ms { funcs = f fs }

projFromVars f ms@(TCS {vars = vs})    = ms { vars = f vs }



------------
-- the toplevel checking of a Program
------------
checkProg :: T.Prog -> StateWithErr Im.Prog
checkProg (T.Prog (T.Ident id) decs) = do mapM checkDec decs
                                          TCS { funcs=fs,
                                                types=ts } <- St.get
                                          return (Im.Prog id (Im.ProgTables {Im.funcs=fs,
                                                                             Im.types=ts}))




-- checkDec only adds stuff to our tables, consts, types, etc; doesn't
-- return anything
-- ::::::::::::::::
checkDec :: T.Dec -> StateWithErr ()
-- ::::::::::::::::


-- addendum - we want to flag global variables
checkDec dec@(T.VarDecl t ids) = do im_t <- checkTyp t
                                    isGlobal <- atGlobalScope
                                    let flags = if isGlobal then [Im.Global] else []
                                    mapM_ (\(T.Ident id) -> addToVars im_t flags id) ids

checkDec dec@(T.FunDecl t id@(T.Ident name) args decs stms) =
    do im_t <- checkTyp t
       -- now prepare the new scope for this function, by pushing a new empty
       -- Map to the top of the SymbolTable stack
       pushScope
       -- check the formal args
       im_args <- mapM checkTypedName args
       -- add the formal argument variables to the scope
       mapM_ addArgVar im_args
       -- grab the table with just args
       arg_tab <- peekScope
       -- add the function name as a variable
       addToVars im_t [Im.RetVar] name
       mapM checkDec decs
       im_stms <- mapM checkStm stms
       var_tab <- popScope
       -- add this function's type
       addToTypes name (Im.FuncT im_t (map snd im_args))
       -- for the local-var initialization (mkVarInits), we need to
       -- pass the function scope without the arg variables, hence the
       -- Map.difference call
       init_stms <- mkVarInits (Map.difference var_tab arg_tab)
       -- and add the actual function
       addToFuncs name (Im.Func name
                                (mkVarSet var_tab)
                                im_t
                                im_args
                                (init_stms ++ im_stms))

    where addArgVar  (name,typ) = addToVars typ [Im.FormalParam] name
          tn2var     (name,_)   = (Im.VFlagged [Im.FormalParam] (Im.VSimple name))



-- special treatment for an Enum type declaration
checkDec (T.TypeDecl (T.Ident name) t@(T.EnumT ids)) =
    do (Im.EnumT _ size) <- checkTyp t
       let im_t = (Im.EnumT name size)
       -- add all the id's as instances of im_t, with an Immutable flag
       mapM (addToVars im_t [Im.Immutable] . extr) ids
       addToTypes name im_t
    where extr (T.Ident id) = id

checkDec (T.TypeDecl n@(T.Ident name) t) = do im_t <- checkTyp t
                                              addToTypes name im_t


checkDec (T.ConstDecl (T.Ident name) (T.EInt i)) =
    do St.modify $ projFromConst $ Map.insert name i

checkDec (T.ConstDecl (T.Ident name) _)          =
    throwErr 42 $ "Bad value for const " << name




-- :::::::::::::::::::::
checkStm :: T.Stm -> StateWithErr Im.Stm
-- :::::::::::::::::::::

checkStm (T.SBlock decs stms) = do pushScope
                                   mapM checkDec decs
                                   new_stms <- mapM checkStm stms
                                   var_tab <- popScope
                                   init_stms <- mkVarInits var_tab
                                   return $ Im.SBlock (mkVarSet var_tab)
                                                      (init_stms ++ new_stms)


checkStm (T.SAss lval val)     = do lval_new <- checkLVal lval
                                    val_new  <- checkExp val
                                    return $ Im.SAss lval_new val_new


checkStm s@(T.SFor cnt@(T.Ident cnt_str) lo hi stm) =
    do new_lo <- checkExp lo
       new_hi <- checkExp hi
       pushScope
       checkLoopCounter s cnt_str
       -- add an Int to the scope symbol table for the loop counter
       -- give it a LoopCounter and Immutable flag
       let flags = [Im.LoopCounter, Im.Immutable]
       addToVars (Im.IntT (Im.UnOp Im.Bitsize (new_hi - new_lo)))
                 flags
                 cnt_str
       new_stm <- checkStm stm
       popScope
       let counterVar = (Im.VFlagged flags (Im.VSimple cnt_str))
       return (Im.SFor counterVar new_lo new_hi [new_stm])
                                   

checkStm s@(T.SIf cond stm) =
    do new_cond@(Im.ExpT t _) <- checkExp cond
       case t of
              (Im.BoolT) -> return ()
              _          -> throwErr 42 $ "In if statement " << s << ", " << cond
                                      << " is not boolean"
       new_stm <- checkStm stm
       return $ Im.SIfElse new_cond (extractLocals new_stm, [new_stm]) (Cont.empty, [])

-- reuse the above code a bit...
checkStm s@(T.SIfElse cond stm1 stm2) =
    do (Im.SIfElse new_cond (locals1,new_stm1s) _) <- checkStm (T.SIf cond stm1)
       new_stm2 <- checkStm stm2
       let locals2 = extractLocals new_stm2
       return $ Im.SIfElse new_cond (locals1, new_stm1s)
                                    (locals2, [new_stm2])



---------------
-- checkTyp translates to Im.Typ, and does cursory checking to make
-- sure referenced type names are defined already
-- computes the sizes of Int and Array, which should be statically
-- computable at this stage
-- TODO: when we consider .bitSize etc. expressions, will need to
-- delay the computation of type sizes till Unroll
---------------
checkTyp :: T.Typ -> StateWithErr Im.Typ

checkTyp (T.StructT fields)     = do im_fields <- mapM checkTypedName fields
                                     return (Im.StructT im_fields)
                                    
checkTyp (T.ArrayT t sizeExp)   = do im_t <- checkTyp t
                                     im_size <- checkExp sizeExp
                                     int_size <- lift $ Im.evalStatic im_size
                                     return (Im.ArrayT im_t (Im.lint int_size))

checkTyp (T.SimpleT (T.Ident name)) = do lookupType name
                                         return (Im.SimpleT name)
                                          
checkTyp (T.IntT i)             = do new_i <- checkExp i
                                     int_i <- lift $ Im.evalStatic new_i
                                     return $ intlitType int_i
-- FIXME: this should only occur in the context of function
-- parameters, should check it:
checkTyp (T.GenIntT)            = return Im.GenIntT
checkTyp T.BoolT                = return Im.BoolT
checkTyp T.VoidT                = return Im.VoidT
-- do a partial processing of EnumT, without sticking its name in
-- there, this will be done by the parent checkDec
checkTyp (T.EnumT ids)          = return (Im.EnumT "" (bitsize $ length ids))


---------------------
-- expandTyp does top-level resolving of type names. No need to fully unroll a
-- complex type
---------------------
expandTyp :: Im.Typ -> StateWithErr Im.Typ
expandTyp (Im.SimpleT name)     = do exp_t <- lookupType name
                                     expandTyp exp_t
expandTyp t                     = return t



-- type of a literal int
intlitType = Im.IntT . Im.lint

-- the bitsize of an integer is at least one
bitsize :: (Integral a, Integral b) => a -> b
bitsize = (max 1) . ilog2


----------
-- checkExp will always return an annotated Exp (constructed with ExpT),
-- which can then be matched on
----------

-- ::::::::::::::::
checkExp :: T.Exp -> StateWithErr Im.Exp
-- ::::::::::::::::


-- for all these Exp's, want to figure out which are const and 
checkExp e@(T.EIdent (T.Ident nm)) =
    do ent <- extractEnt e nm
       case ent of
                -- same as a literal int!
                EntConst i         -> checkExp (T.EInt i)
                -- a var is usually variable, but loop counters will be static
                EntVar (typ,v)
{-
-- this causes a problem when checkExp is called in an lval context                    
                    | elem Im.RetVar (Im.vflags v) ->
                        throwErr 42 $ "Cannot take value of return variable " << v
-}
                    | otherwise ->
                        return $ annot typ $ if elem Im.LoopCounter (Im.vflags v)
                                             then Im.EStatic (Im.EVar v)
                                             else Im.EVar v
                _                  ->
                    throwErr 42 $ "Identifier " << nm << " is illegal in expression " << e



checkExp e@(T.EInt i)        = return $ annot (intlitType (bitsize i))
                                                (fromInteger i)

checkExp T.ETrue             = return $ annot Im.BoolT (Im.ELit $ Im.LBool True)
checkExp T.EFalse            = return $ annot Im.BoolT (Im.ELit $ Im.LBool False)

-- here we also deal with bit-access on ints, which has the same concrete
-- syntax as array access
checkExp e@(T.EArr arr idx)    = do new_arr <- checkExp arr
                                    new_idx <- checkExp idx
                                    -- check some error conditions, and get the
                                    -- element type
                                    (elemT,new_e) <- check new_arr new_idx
                                    return $ annot elemT new_e
    -- returns the type of the array elements
    where check arr@(Im.ExpT (Im.ArrayT typ _) _) idx = do checkIdx idx
                                                           return ( typ,        (Im.EArr arr idx) )
          check arr@(Im.ExpT (Im.IntT   _)     _) idx = do checkIdx idx
                                                           return ( (Im.IntT 1), (Im.EGetBit arr idx) )
          check _ _                                   = throwErr 42 $ "Array " << arr << " in " << e
                                                                     << " is not an array or int!"
          -- index type has to be int
          checkIdx idx = case idx of
                           (Im.ExpT (Im.IntT _) _)  -> return ()
                           _                        -> throwErr 42 $ "Array index in " << e << " is not an Int"


checkExp e@(T.EStruct str field@(T.EIdent (T.Ident fieldname)))
    = do new_str@(Im.ExpT strT _) <- checkExp str
         typ_full                <- expandTyp strT
         (typ,new_e)             <- check typ_full new_str
         return $ annot typ new_e

         -- ** check: return the expression's type and value
         -- find the field in this struct's definition
    where check (Im.StructT fields) new_str =
              do case lookup fieldname fields of
                          (Just t) -> do size   <- typeLength t
                                         
                                         offset <-    liftM sum $
                                                      mapM (typeLength . snd) $
                                                      takeWhile ( (/= fieldname) . fst ) $
                                                      fields
                                         return (t, Im.EStruct new_str (offset,size))
                          _        -> throwErr 42 $ "in " << e << ", struct has no field "
                                                      << fieldname

          -- if it's an i.bitSize expression, just return an Int
          check (Im.IntT size) _
              | fieldname == "bitSize" = do return ( Im.IntT (Im.UnOp Im.Bitsize size), size )

          -- for bitSize on a "generic" int (type Int<`>), we need to add EBitsize
          -- expressions, to be evaluated when functions are inlined.
          check (Im.GenIntT) new_str = let val = Im.EStatic $ Im.UnOp Im.Bitsize new_str
                                       in  return (Im.IntT (Im.UnOp Im.Bitsize val) , val)

          check typ _              = throwErr 42 $ str << " in " << e << " is not a struct, it is " << typ



checkExp e@(T.EStruct str _) = throwErr 42 $ "struct field in " << e << " is not a simple name"


-- to check a function call:
-- - check and get types for all the args
-- - compare those to the function's formal params
-- TODO: not finished with the checking here! need to check that the
-- actual args in the call have the correct types
checkExp e@(T.EFunCall (T.Ident fcnName) args) =
    do (Im.Func _ _ t _ _)      <- extractFunc e fcnName
       im_args                  <- mapM (checkExp . extrExp) args
       return (Im.ExpT t (Im.EFunCall fcnName im_args))
    where extrExp (T.FunArg e) = e


checkExp e
    | Just (_, op, e1, e2) <- analyzeBinExp e
         = do new_e1 <- checkExp e1
              new_e2 <- checkExp e2
              (Im.ExpT t new_e) <- case classifyBinExp e of
                                      Arith      -> checkArith op new_e1 new_e2
                                      Binary     -> checkBinary op new_e1 new_e2
                                      Logical    -> checkLogical op new_e1 new_e2
                                      Comparison -> checkComparison op new_e1 new_e2
              let static_e = case (all isStaticExp [new_e1, new_e2]) of
                               True  -> Im.ExpT t (Im.EStatic new_e)
                               False -> Im.ExpT t             new_e
              return static_e
    | Just (op, e) <- analyzeUnaryOp e       = do new_e <- checkExp e
                                                  typ <- checkUnary new_e
                                                  return $ annot typ new_e
    where isStaticExp (Im.EStatic _)    = True
          isStaticExp _                 = False



-- return the length, in simple types (ie Int and Bool), of a type
typeLength :: Im.Typ -> StateWithErr Int
typeLength t =
    case t of
      (Im.StructT fields)       -> mapM (typeLength . snd) fields >>=
                                   return . sum
      (Im.SimpleT name)         -> lookupType name >>=
                                   typeLength
      (Im.RedArrayT typ len)    -> typeLength typ >>=
                                   return . (* len)
      _                         -> return 1
--      (ArrayT typ len_e)  = typeLength typ
      
                                          
                 

checkTypedName :: T.TypedName -> StateWithErr (Im.Ident, Im.Typ)
checkTypedName (T.TypedName t (T.Ident name)) = do t_new <- checkTyp t
                                                   return (name, t_new)




-- make sure an LVal is actually assignable
checkLVal :: T.LVal -> StateWithErr Im.Exp

-- an identifier needs special treatment: don't assign to consts etc; only
-- assign to variables
checkLVal lv@(T.LVal (T.EIdent (T.Ident name))) =
    do ent <- extractEnt lv name
       case ent of
          (EntVar (_,v))
              | not $ elem Im.Immutable (Im.vflags v) 
                  -> return (Im.EVar v)
          _       -> throwErr 42 $ "Assigning to immutable value " << name

checkLVal (T.LVal e) =
    do im_e <- case e of
                   e@(T.EStruct _ _) -> checkExp e
                   e@(T.EArr _ _)    -> checkExp e
                   _                 -> throwErr 42 $ e << " is not an lvalue"
       return im_e





checkLoopCounter :: (Show a) => a -> Im.Ident -> StateWithErr ()
checkLoopCounter ctx name =
    -- if extractEnt returns an error, that's fine, we don't want to
    -- propagate it, hence we replace it with a dummy Entity value
    -- (EntConst 0)
    do var <- extractEnt ctx name `catchError` (const $ return $ EntConst 0)
       case var of
         (EntVar (_,v))
             | elem Im.LoopCounter (Im.vflags v)  -> throwErr 42 $ "Loop counter " << name
                                                                << " reused"
         _                                        -> return ()
         

-- create initialization statements for local variables
-- needs to be in StateWithErr because of (typeLength)
mkVarInits :: Im.VarTable -> StateWithErr [Im.Stm]
mkVarInits local_table = do let locs  = map snd $ Map.toList local_table
                            concatMapM mkInit $ map (\(t,v) -> (t,Im.EVar v)) locs
          -- create Init statements for an lval of the given type
    where mkInit (t,lval) = trace ("mkInit " << lval << "::" << t) $
                            case t of
                              (Im.IntT i)          -> return [ass lval 0 t]
                              (Im.BoolT)           -> return [ass lval (Im.lbool False) t]
          -- for a struct, we first add an SAss for the whole struct,
          -- using Im.EComplexInit,
          -- and then individual SAss for each member, recursively
                              (Im.StructT flds)    ->
                                  do lens <-   mapM (typeLength . snd) flds
                                     let offsets = init $ scanl (+) 0 lens
                                     inits <- zipWithM (mkStruct lval)
                                                       flds
                                                       (zip offsets lens)
                                     return $ (ass lval (Im.EComplexInit (sum lens)) t) :
                                              concat inits

                              (Im.ArrayT elem_t len)    ->
                                  do return [ass lval (Im.EComplexInit 1) t]

                              (Im.SimpleT tname)    ->
                                  do typ <- lookupType tname
                                     mkInit (typ, lval)
                              _                     -> return []

          ass lval val t = Im.SAss lval (Im.ExpT t val)
          -- create a field-init statement for struct 'str', for the
          -- given field (ie. its type, offset and length)
          mkStruct str (_, t) (offset,len) = mkInit (t, (Im.EStruct str (offset,len)))



-- extract the local variables declared below the given statement,
-- (ie. declared in SBlock blocks)
extractLocals :: Im.Stm -> Im.VarSet
extractLocals stm = let (_, varsets) = runWriter (extractLocals' stm)
                    in  foldr Cont.union Cont.empty varsets

extractLocals' :: Im.Stm -> Writer [Im.VarSet] Im.Stm
extractLocals' = Im.mapStmM f_s f_e
    where f_s s@(Im.SBlock vars _) = do tell [vars]
                                        return s
          f_s s                 = return s

          f_e                   = myLiftM id



-- at the global scope, the VarTable has only one level
atGlobalScope = do TCS {vars=vs} <- St.get
                   return (length vs == 1)


-- extractor of different table objects
data Entity = EntConst Integer
            | EntFunc  Im.Func
            | EntVar   (Im.Typ,Im.Var)
            | EntType  Im.Typ

extractEnt :: (Show a) => a -> Im.EntName -> StateWithErr Entity
extractEnt ctx name = do TCS {types=ts,
                              consts=cs,
                              funcs=fs,
                              vars=vs} <- St.get
                         case extractHelper (ts,cs,fs,vs) name of
                           (Just ent)   -> return ent
                           _            -> throwErr 42 $ "Entity " << name
                                                           << " not in scope in "
                                                           << ctx

-- the first one that succeeds will be the result (which is what msum
-- does in the Maybe monad)
-- extractHelper :: ... -> Maybe Entity
extractHelper (ts,cs,fs,vs) name = msum [(maybeLookup name vs >>=
                                          return . EntVar),
                                         (do res <- maybeLookup name [ts]
                                             return (EntType res)),
                                         (do res <- maybeLookup name [cs]
                                             return (EntConst res)),
                                         (do res <- maybeLookup name [fs]
                                             return (EntFunc res))]


extractFunc ctx name = do TCS {funcs=fs} <- St.get
                          let res = maybeLookup name [fs]
                          case res of (Just f) -> return f
                                      _        -> throwErr 42 $ name << " in " << ctx
                                                              << " is not in scope as a function"

-- check a unary opeartion, and return its type.
-- here 'e' is the whole unary expression, not just the parameter
checkUnary e@(Im.ExpT t inner_e) =
         do t_full <- expandTyp t
            case (inner_e, t_full) of
                         ( (Im.UnOp Im.Not _),     (Im.BoolT) )   -> return Im.BoolT
                         ( (Im.UnOp Im.BNot _), it@(Im.IntT _) )  -> return it
                         ( (Im.UnOp Im.Neg  _), it@(Im.IntT _) )  -> return it
                         (_           , _             )  -> throwErr 42 $ "Unary operation "
                                                                         << e << " has invalid param type"
                                 


-- check a logical expression. quite easy
checkLogical op e1@(Im.ExpT t1 _) e2@(Im.ExpT t2 _) =
    do t1_full <- expandTyp t1
       t2_full <- expandTyp t2
       case (t1_full, t2_full) of
                (Im.BoolT, Im.BoolT) -> return (Im.ExpT Im.BoolT (Im.BinOp op e1 e2))
                _                    -> throwErr 42 $ "Logical expression " << (Im.BinOp op e1 e2)
                                                   << " does not have Bool args"

-- check an arithmetic expression whose components e1 and e2 are already checked.
checkBinary op e1 e2 =
    do let ( (Im.ExpT t1 _), (Im.ExpT t2 _) ) = (e1,e2)
       t1_full <- expandTyp t1
       t2_full <- expandTyp t2
       case (t1_full,t2_full) of
             ((Im.IntT i1),
              (Im.IntT i2) )    -> return $ annot (Im.IntT $ Im.BinOp Im.Max i1 i2) (Im.BinOp op e1 e2)
             _                  -> throwErr 42 $ "Arithmetic expression "
                                            << (Im.BinOp op e1 e2)
                                            << " has non-integer params"

-- same as checkBinary except we have to increment the bitsize of the
-- output by 1
checkArith op e1 e2 = do (Im.ExpT (Im.IntT i) e) <- checkBinary op e1 e2
                         return $ Im.ExpT (Im.IntT (i+1)) e
{-
checkArith op e1 e2 =
    case (e1,e2) of
       ( (T.IntT i1) _),
         (T.ExpT (T.IntT i2) _) )    -> return ( (op e1 e2), (T.EMax i1 i2) )
       _                             -> throwErr 42 $ "Arithmetic expression "
                                                      << (op e1 e2)
                                                      << " has non-integer params"
-}

-- the parameters must have the same type, and can be Bool, Int or Enum
checkComparison op e1@(Im.ExpT t1 _) e2@(Im.ExpT t2 _) =
    do full_t1 <- expandTyp t1
       full_t2 <- expandTyp t2
       let answer = (Im.ExpT Im.BoolT (Im.BinOp op e1 e2))
       case (full_t1, full_t2) of
          (Im.BoolT,  Im.BoolT)                         -> return answer
          (Im.IntT _, Im.IntT _)                        -> return answer
          (Im.EnumT nm1 _, Im.EnumT nm2 _) | nm1 == nm2 -> return answer
          _                      -> throwErr 42 $ "Args to comparison expression "
                                                  << answer
                                                  << " are invalid"


mkVarSet :: Im.VarTable -> Im.VarSet
-- toAscList gives a sorted list of (key,value), and the values are
-- (typ,var), thus (snd . snd) to extract the var
mkVarSet = Cont.fromList . (map (snd . snd)) . Map.toAscList

----------------------------------------------------------
----------------------------------------------------------



-- push and pop a SymbolTable scope
pushScope = St.modify $ projFromVars $ (push Map.empty)
-- want to return the scope here
popScope  = do ms@(TCS {vars = vs}) <- St.get
               let scope = peek vs
               St.put (ms {vars = pop vs})
               return scope
-- return the current top scope
peekScope = do TCS {vars = vs} <- St.get
               return $ peek vs

               
------------------------------
-- compute a static expression
------------------------------
computeStaticExp :: T.Exp -> StateWithErr Integer
computeStaticExp e
    | Just (_, op, e1, e2) <- analyzeBinExp e = do i1 <- computeStaticExp e1
                                                   i2 <- computeStaticExp e2
                                                   return $ doOp e i1 i2
    | otherwise                              = 
        case e of
               (T.EInt i)       -> return i
               (T.EIdent (T.Ident nm)) ->
                   do -- has to be a constant, look in the ConstTable
                     TCS {consts=ct} <- St.get
                     let val = maybeLookup nm [ct]
                     case val of
                              (Just i) -> return i
                              _        -> throwErr 42 $ "Static expression " << e << " not static: " << nm
               _                -> throwErr 42 $ "Invalid static expression " << e



addToFuncs :: Im.EntName -> Im.Func -> StateWithErr ()
addToFuncs name f = St.modify $
                    projFromFuncs $
                    Map.insert name f

addToTypes :: Im.EntName -> Im.Typ -> StateWithErr ()
addToTypes name typ = St.modify $
                      projFromTypes $
                      Map.insert name typ


-- take care of constructing a VFlagged if there are flags
addToVars typ flags name = let v' = (Im.VSimple name)
                               v  = if null flags
                                    then v'
                                    else (Im.VFlagged flags v')
                           in St.modify $
                              projFromVars $
                              modtop $
                              Map.insert name (typ,v)


lookupType :: String -> StateWithErr Im.Typ
lookupType name =
     do TCS {types=ts} <- St.get
        let res = maybeLookup name [ts]
        case res of
           Just t -> return t
           _      -> throwErr 42 $ "Type " << name << " is not in scope"


-- getRvalTyp name     = lookupSym Var name


                      
-- annotate an expression
annot t e = Im.ExpT t e




-- also takes a predicate which is applied to the exp, and only matching exps are returned
analyzeSomeBinExps pred e = if pred e then analyzeBinExp e else Nothing

analyzeBinExp e = case e of
                     (T.EPlus e1 e2)      -> Just (Arith,Im.Plus,e1,e2)
                     (T.ETimes e1 e2)     -> Just (Arith,Im.Times,e1,e2)
                     (T.EMinus e1 e2)     -> Just (Arith,Im.Minus,e1,e2)
                     (T.EDiv e1 e2)       -> Just (Arith,Im.Div,e1,e2)

                     (T.EEq e1 e2)      -> Just (Comparison,Im.Eq,e1,e2)
                     (T.ENeq e1 e2)     -> Just (Comparison,Im.Neq,e1,e2)
                     (T.ELt e1 e2)     -> Just (Comparison,Im.Lt,e1,e2)
                     (T.EGt e1 e2)     -> Just (Comparison,Im.Gt,e1,e2)
                     (T.EGtEq e1 e2)     -> Just (Comparison,Im.GtEq,e1,e2)
                     (T.ELtEq e1 e2)     -> Just (Comparison,Im.LtEq,e1,e2)

                     (T.EAnd e1 e2)    -> Just (Logical,Im.And,e1,e2)
                     (T.EOr e1 e2)     -> Just (Logical,Im.Or,e1,e2)

                     (T.ESL e1 e2)        -> Just (Binary,Im.SL,e1,e2)
                     (T.ESR e1 e2)        -> Just (Binary,Im.SR,e1,e2)
                     (T.EBOr e1 e2)        -> Just (Binary,Im.BOr,e1,e2)
                     (T.EBAnd e1 e2)        -> Just (Binary,Im.BAnd,e1,e2)
                     (T.EBXor e1 e2)        -> Just (Binary,Im.BXor,e1,e2)

                     _                  -> Nothing

analyzeUnaryOp e = case e of
                    (T.ENeg e1)          -> Just (Im.Neg, e1)
                    (T.ENot e1)          -> Just (Im.Not, e1)
                    (T.EBNot e1)         -> Just (Im.BNot, e1)
                    _                   -> Nothing


classifyBinExp e = case analyzeBinExp e of
                                        Just (kind,_,_,_) -> kind
                                        _                 -> NotBin

                     

-- some predicates
isArithOp = (== Arith) . classifyBinExp
isCompOp = (== Comparison) . classifyBinExp
isLogicOp = (== Logical) . classifyBinExp



-- use a whole T.Exp to indicate the operator so we can reuse those values
doOp :: T.Exp -> Integer -> Integer -> Integer
doOp (T.EPlus _ _) = (+)
doOp (T.EMinus _ _) = (-)
doOp (T.ETimes _ _) = (*)



testDecs = [ T.ConstDecl (T.Ident "x") (T.EInt 10),
             T.TypeDecl  (T.Ident "y") T.BoolT ]
