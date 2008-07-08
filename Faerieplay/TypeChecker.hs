{-# OPTIONS_GHC -fallow-overlapping-instances -fglasgow-exts -cpp #-}
-- -fglasgow-exts for parallel list comprehension

--
-- Circuit compiler for the Faerieplay hardware-assisted secure
-- computation project at Dartmouth College.
--
-- Copyright (C) 2003-2007, Alexander Iliev <sasho@cs.dartmouth.edu> and
-- Sean W. Smith <sws@cs.dartmouth.edu>
--
-- All rights reserved.
--
-- This code is released under a BSD license.
-- Please see LICENSE.txt for the full license and disclaimers.
--


-- code to split the AST into several namespaces of global bindings:
-- constants, types, variables and functions

module Faerieplay.TypeChecker
    (
     typeCheck,
     name2var
    )
    where

import Monad    (foldM, msum, liftM, zipWithM)
import List     (find, findIndex)
import Maybe    (fromJust)

-- import Debug.Trace      (trace)

import qualified Data.Map as Map
import qualified Control.Monad.State as St --- (MonadState, State, StateT, modify, runStateT)
import Control.Monad.Writer         (Writer, runWriter, tell)
import Control.Monad.Error          (Error, MonadError,
                                     throwError, catchError, noMsg, strMsg,ErrorT(..))
import Control.Monad.Trans (lift)

import Faerieplay.Common (MyError(..),MyErrorCtx, ErrCtxMonad, trace, logDebug)

import Faerieplay.SashoLib ((<<), ilog2,
                 myLiftM, concatMapM, iterateWhileM,
                 (>>==), mapTuple2, projSnd,
                 StreamShow(..),
                 compilerAssert, throwCompilerErr
                )

import Faerieplay.Stack               as Stack (Stack(..), maybeLookup,modify_first_map)

import qualified Faerieplay.Container as Cont

import qualified Faerieplay.ErrorWithContext                   as EWC

--
-- the abstract syntax tree description
-- NOTE: this is where the syntax used is decided.
--
#if defined SYNTAX_C

import qualified Faerieplay.Bnfc.Fcpp.Abs as T
import qualified Faerieplay.Bnfc.Fcpp.Print as Print

#elif defined SYNTAX_SFDL

import qualified Faerieplay.Bnfc.Sfdl.Abs as T
import qualified Faerieplay.Bnfc.Sfdl.Print as Print

#else

#error No syntax set via the SYNTAX preprocessor value

#endif


import qualified Faerieplay.Intermediate as Im


type TypeError = MyErrorCtx





-- we go with integer consts for now
type ConstTable = Map.Map Im.EntName Integer


data TCState = TCS { vars   :: [Im.VarTable],
                     types  :: Im.TypeTable,
                     consts :: ConstTable,
                     funcs  :: Im.FuncTable,
                     func_stack :: [(String,Im.Typ)] -- ^ Stack of function names being
                                                  -- defined, and the
                                                  -- function's return type
                   }
    deriving (Show,Eq)



type MyStateT = TCState


type StateWithErr = ErrorT MyErrorCtx (St.State MyStateT)


throwErr :: Int -> String -> StateWithErr a
throwErr p msg = throwError $ EWC.EWC (Err p msg, [])

-- TODO: implement real line number reporting. In the meantime, use a clearly nonsensical
-- line number everywhere.
cL = -42


instance Im.TypeTableMonad StateWithErr where
    getTypeTable = St.gets types



-- here we will compose the Error and State monads with the state monad inside the error
-- transformer.
-- main reason is because the Error context manipluation operator (<?>) needs direct
-- access to the error parameter.
typeCheck :: T.Prog -> ErrCtxMonad Im.Prog
typeCheck p = let startState = TCS { vars   = [Map.empty],
                                     consts = Map.empty,
                                     funcs  = Map.empty,
                                     types  = Map.empty,
                                     func_stack = Stack.empty
                                   }
--                  out = St.runStateT (checkProg p) startState
                  -- we do not need the end state.
                  (val_or_err,state') = St.runState (runErrorT $ checkProg p) startState
              in  val_or_err





-- take a function which takes and returns the ConstTable, and project it onto
-- the whole MyStateT, keeping the other fields fixed
-- projToConst :: (ConstTable -> ConstTable) -> MyStateT -> MyStateT
projToConst  f ms@(TCS {consts = ct})   = ms { consts = f ct }

projToTypes f ms@(TCS {types = ts})    = ms { types = f ts }

projToFuncs f ms@(TCS {funcs = fs})    = ms { funcs = f fs }

projToVars f ms@(TCS {vars = vs})    = ms { vars = f vs }

projToFuncStack f ms@(TCS {func_stack = x})    = ms { func_stack = f x }


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
checkDec dec@(T.VarDecl t ids) = setContext dec $
                                 do im_t        <- checkTyp t
                                    isGlobal    <- atGlobalScope
                                    let flags   = if isGlobal then [Im.Global] else []
                                    mapM_ (\(T.Ident id) -> addToVars im_t flags id) ids

checkDec dec@(T.FunDecl t id@(T.Ident name) args decs stms) =
    setContext dec $
    do im_t <- checkTyp t
       -- now prepare the new scope for this function, by pushing a new empty
       -- Map to the top of the SymbolTable stack
       pushScope
       -- push the name on to the func name stack
       St.modify (projToFuncStack $ push (name,im_t))
       -- check the formal args
       im_args <- mapM checkTypedName args
       -- add the formal argument variables to the scope
       mapM_ addArgVar im_args
       -- grab the table with just args
       arg_tab <- peekScope
       -- add the function name as a variable
       -- FIXME: this may not be good, as it will generate a (probably unwanted) array
       -- init statement. more generally, may have large runtime overhead which is not
       -- needed.
       addToVars im_t [Im.RetVar] name
       mapM checkDec decs
       im_stms <- mapM checkStm stms
       St.modify (projToFuncStack pop)
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
checkDec d@(T.TypeDecl (T.Ident name) t@(T.EnumT ids)) =
    setContext d$
    do (Im.EnumT _ size) <- checkTyp t
       let im_t = (Im.EnumT name size)
       -- add all the id's as instances of im_t, with an Immutable flag
       mapM (addToVars im_t [Im.Immutable] . extr) ids
       addToTypes name im_t
    where extr (T.Ident id) = id

checkDec (T.TypeDecl n@(T.Ident name) t) = do im_t <- checkTyp t
                                              addToTypes name im_t


checkDec d@(T.ConstDecl (T.Ident name) val) =
    setContext d$
    do im_val   <- checkExp val
       int_val  <- Im.evalStatic im_val
       St.modify $ projToConst $ Map.insert name int_val


-- :::::::::::::::::::::
checkStm :: T.Stm -> StateWithErr Im.Stm
-- :::::::::::::::::::::

checkStm s@(T.SBlock decs stms)
                              = setContext s $
                                do pushScope
                                   mapM checkDec decs
                                   new_stms <- mapM checkStm stms
                                   var_tab <- popScope
                                   init_stms <- mkVarInits var_tab
                                   return $ Im.SBlock (mkVarSet var_tab)
                                                      (init_stms ++ new_stms)


checkStm s@(T.SAss lval val)   = setContext s $
                                 do lval_new <- checkLVal lval
                                    val_new  <- checkExp val
                                    [lv', v'] <- mapM (canonicalize . Im.getExpTyp) [lval_new, val_new]
                                    if lv' == v'
                                       then return $ Im.SAss lval_new val_new
                                       else throwErr cL ("Type mismatch in assignment; types are "
                                                         << lv' << "(lvalue) and " << v' << "(rvalue)")


checkStm s@(T.SPrint prompt vals)
                                = setContext s $
                                  do vals_new   <- mapM checkExp vals
                                     (mapM checkLVal $ map T.LVal vals)
                                        `catchError`
                                        (\e -> throwErr
                                               cL
                                               "Arguments to print() must be legal lval's")
                                     return $ Im.SPrint prompt vals_new



checkStm s@(T.SFor cnt@(T.Ident cnt_str) lo hi stm) =
    setContext s $
    do begin    <- checkExp lo >>= Im.evalStatic
       end      <- checkExp hi >>= Im.evalStatic
       -- can have the loop count forwards and backwards by 1
       let countVals
               | begin <= end   = [begin..end]
               | otherwise      = reverse [end..begin]
       pushScope
       checkLoopCounter cnt_str
       -- add an Int to the scope symbol table for the loop counter
       -- give it a LoopCounter and Immutable flag
       let flags = [Im.LoopCounter, Im.Immutable]
       addToVars (Im.IntT (Im.UnOp Im.Bitsize (Im.lint $ toInteger $ length countVals)))
                 flags
                 cnt_str
       new_stm <- checkStm stm
       popScope
       let counterVar = (Im.VFlagged flags (Im.VSimple cnt_str))
       return (Im.SFor counterVar countVals [new_stm])


#ifdef SYNTAX_C
-- the for-loop coming from the C front end
checkStm s@(T.SFor_C cnt@(T.Ident cnt_str)
                     start_e
                     stop_cond
                     update_ass
                     stm)                           =
    setContext s $
    do checkLoopCounter cnt_str -- checks if the counter entity is also a counter var in
                                -- an enclosing loop - don't see when that would make
                                -- sense.
       new_start_e      <- checkExp start_e
       new_stop_cond    <- checkExp stop_cond
       new_update_ass   <- checkAssStm update_ass

       let flags        = [Im.LoopCounter]
           ctrvar       = (Im.VFlagged flags (Im.VSimple cnt_str))
       -- the loop counter must already be in the symbol table (already declared). but,
       -- now need to add flags to it.
       modVar cnt_str (Im.add_vflags flags)

       countVals        <- getCountVals ctrvar new_start_e new_stop_cond new_update_ass

       pushScope
       new_stm          <- checkStm stm
       popScope

       return (Im.SFor ctrvar countVals [new_stm])

    where
          getCountVals countVar begin_exp stop_cond update =
              do begin      <- Im.evalStatic begin_exp
                 -- FIXME: if the provided stop condition leads to infinite looping,
                 -- this will loop too; should install a max count check 
                 vals       <- iterateWhileM (keepgoing countVar stop_cond)
                                             (nextVal countVar update)
                                             begin
                 return vals
                 

          nextVal countVar (Im.AssStm lval rval) x =
              do let rval'  = Im.substExp countVar (Im.lint x) rval
                 lval'      <- Im.evalStatic rval'
                 return lval'

          keepgoing countVar stop_cond x           =
              do let cond   = Im.substExp countVar (Im.lint x) stop_cond
                 cond_val   <- Im.evalStatic cond
                 return (toEnum $ fromIntegral cond_val)
                            `logDebug`
                            ("keepgoing (x=" << x << "): cond_val = " << cond_val)

-- SYNTAX_C
#endif




checkStm s@(T.SIf cond stm) =
    setContext s $
    do new_cond@(Im.ExpT t _) <- checkExp cond
       case t of
              (Im.BoolT) -> return ()
              _          -> throwErr cL $ "Condition \"" << cond << "\" is not a boolean"
       new_stm <- checkStm stm
       return $ Im.SIfElse new_cond (extractLocals new_stm, [new_stm]) (Cont.empty, [])

-- reuse the above code a bit...
checkStm s@(T.SIfElse cond stm1 stm2) =
    setContext s $
    do (Im.SIfElse new_cond (locals1,new_stm1s) _) <- checkStm (T.SIf cond stm1)
       new_stm2 <- checkStm stm2
       let locals2 = extractLocals new_stm2
       return $ Im.SIfElse new_cond (locals1, new_stm1s)
                                    (locals2, [new_stm2])

#ifdef SYNTAX_C
-- | replace 'return' with an assignment to a var with same name as enclosing function.
checkStm s@(T.SReturn exp)              =
    setContext s $
    do new_exp      <- checkExp exp
       -- find the enclosing function
       func_stack   <- St.gets func_stack
       (fname,t)    <- if Stack.isEmpty func_stack
                       then throwErr cL $ "return called while not inside a function definition"
                       else return $ Stack.peek func_stack
       -- FIXME: check the type compatibility somehow
       -- ASSUME: the enclosing function def has added the return var to the var table; we
       -- don't do it here.
       return $ Im.SAss (Im.ExpT t (Im.EVar $ Im.add_vflags [Im.RetVar] $ Im.VSimple fname))
                        new_exp





-- there should be only simple ASOpAss left at this point, after the fixup. Also, they
-- should only have assignment op AssID ('=' and not '+=' etc.)
checkAssStm :: T.AssStm -> StateWithErr Im.AssStm
checkAssStm ass@(T.ASOpAss lval op rval) =
    setContext ass $
    do compilerAssert (op /= T.AssId)
                      "Only expect identity assignment operator during type check, not += etc"
       new_lval     <- checkLVal lval
       new_rval     <- checkExp rval
       return $ Im.AssStm new_lval new_rval


checkAssStm s =     throwCompilerErr
                     "Only expect simple assignment type during type check, not x++, ++x, etc"

-- SYNTAX_C
#endif



---------------
-- checkTyp translates to Im.Typ, and does cursory checking to make
-- sure referenced type names are defined already
-- computes the sizes of Int and Array, which should be statically
-- computable at this stage
-- TODO: when we consider .bitSize etc. expressions, will need to
-- delay the computation of some type sizes till Unroll
---------------
checkTyp :: T.Typ -> StateWithErr Im.Typ

checkTyp t@(T.StructT fields)   = setContext t$
                                  do im_fields      <- mapM checkTypedName fields
                                     -- note that by the time expandType is called here,
                                     -- checkTypedName has already checked the sub-types
                                     -- in the struct fields, so we cannot get an error 
                                     full_ts        <- mapM (Im.expandType [Im.DoAll] . snd)
                                                            im_fields
                                     let lens       = map (Im.typeLength (const 1)) full_ts
                                         bytelens   = map (Im.typeLength Im.tblen) full_ts
                                         offs       = init $ scanl (+) 0 lens
                                         byteoffs   = init $ scanl (+) 0 bytelens
                                         fields'    = (im_fields,
                                                       [Im.FieldLoc { Im.valloc = (off,len),
                                                                      Im.byteloc = (boff,blen) }
                                                        | off <- offs
                                                        | len <- lens
                                                        | boff <- byteoffs 
                                                        | blen <- bytelens])
                                     return $ Im.StructT fields'
                                    
checkTyp t'@(T.ArrayT t sizeExp)= setContext t' $
                                  do im_t       <- checkTyp t
                                     im_size    <- checkExp sizeExp
                                     int_size   <- Im.evalStatic im_size
                                     return (Im.ArrayT im_t (Im.lint int_size))

checkTyp t@(T.SimpleT (T.Ident name))
                                = setContext t $
                                  do lookupType name
                                     return (Im.SimpleT name)
                                          
checkTyp t@(T.IntT i)           = setContext t $
                                  do new_i <- checkExp i
                                     int_i <- Im.evalStatic new_i
                                     return $ intlitType int_i

-- FIXME: this should only occur in the context of function
-- parameters, should check it:
-- checkTyp (T.GenIntT)            = return Im.GenIntT
checkTyp T.BoolT                = return Im.BoolT
checkTyp T.VoidT                = return Im.VoidT
-- do a partial processing of EnumT, without sticking its name in
-- there, this will be done by the parent checkDec
checkTyp (T.EnumT ids)          = return (Im.EnumT "" (bitsize $ length ids))

checkTyp (T.RefT t)             = do im_t  <- checkTyp t
                                     return $ Im.RefT im_t





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
    setContext e $
    do ent <- extractEnt nm
       case ent of
                -- same as a literal int!
                EntConst i         -> checkExp (T.EInt i)
                -- a var is usually variable, but loop counters will be static
                EntVar (typ,v)
{-
-- this causes a problem when checkExp is called in an lval context                    
                    | elem Im.RetVar (Im.vflags v) ->
                        throwErr cL $ "Cannot take value of return variable " << v
-}
                    | otherwise ->
                        return $ annot typ $ if elem Im.LoopCounter $ Im.vflags v
                                             then Im.EStatic $ Im.EVar v
                                             else Im.EVar v
                _                  ->
                    throwErr cL $ "Identifier " << nm << " is invalid in this expression, \
                                                          expected a const or a variable"



checkExp e@(T.EInt i)        = return $ annot (intlitType (bitsize i))
                                                (fromInteger i)

checkExp T.ETrue             = return $ annot Im.BoolT (Im.ELit $ Im.LBool True)
checkExp T.EFalse            = return $ annot Im.BoolT (Im.ELit $ Im.LBool False)

-- here we also deal with bit-access on ints, which has the same concrete
-- syntax as array access
checkExp e@(T.EArr arr idx)    = setContext e $
                                 do new_arr <- checkExp arr
                                    new_idx <- checkExp idx
                                    -- check some error conditions, and get the
                                    -- element type
                                    (elemT,new_e) <- check new_arr new_idx
                                    return $ annot elemT new_e
    -- returns the type of the array elements
    where check arr@(Im.ExpT arr_t _) idx =
              do idx'       <- checkIdx idx
                 -- how much type expansion do we want here?
                 -- looks like just one level, so it resolves a typedef'd Array
                 -- (eg type MyArr = Int[N];)
                 -- but leaves all components intact. Thus, DoTypeDefs without
                 -- DoArrayElems is right.
                 arr_t_full <- Im.expandType [Im.DoTypeDefs] arr_t
                 case (Im.stripRefQual arr_t_full) of
                   (Im.ArrayT typ _)  -> return ( typ,       (Im.EArr arr idx')    )
                   (Im.IntT   _)      -> return ( (Im.IntT 1), (Im.EGetBit arr idx') )
                   t                  -> throwErr cL ("Supposed array " << arr
                                                      << " is not an array or int, it is of type "
                                                      << t)
          -- index type has to be int; annotate the index expression with EStatic if it is
          -- static.
          checkIdx idx = let (Im.ExpT idx_t _) = idx
                         in  do idx_t_full  <- Im.expandType [Im.DoTypeDefs] idx_t
                                -- this isn't actually that useful because we don't know
                                -- at this point where loop counters are, and they are the
                                -- most interesting source of static array indices.
                                idx'        <- (do idx_int <- Im.evalStatic idx
                                                   return $ Im.EStatic $ Im.lint $ idx_int)
                                               `catchError`
                                               -- not static, use idx as is.
                                               (\err -> return idx
                                                        `trace` ("Array index " << idx
                                                                 << " in " << e
                                                                 << " is not static"))
                                case idx_t_full of
                                    (Im.IntT _)  -> return idx'
                                    _            -> throwErr cL ("Array index is not an Int")


checkExp e@(T.EStruct str field@(T.EIdent (T.Ident fieldname)))
    = setContext e $
      do new_str@(Im.ExpT strT _) <- checkExp str
         typ_full                <- Im.expandType [Im.DoTypeDefs, Im.DoRefs] strT
         (typ,new_e)             <- check typ_full new_str
         return $ annot typ new_e

         -- ** check: return the expression's type and value
         -- find the field in this struct's definition
    where check :: Im.Typ       -- the type of the struct
                -> Im.Exp       -- the struct expression
                -> StateWithErr (Im.Typ,     -- the type of the field
                                 Im.Exp)     -- the field expression

          -- same thing for struct and reference to struct
          check (Im.StructT fields_info) new_str            = doStruct fields_info new_str
          check (Im.RefT (Im.StructT fields_info)) new_str  = doStruct fields_info new_str

          -- if it's an i.bitSize expression, just return an Int
          check (Im.IntT size) _
              | fieldname == "bitSize" = do return ( Im.IntT (Im.UnOp Im.Bitsize size), size )

          -- for bitSize on a "generic" int (type Int<`>), we need to add EBitsize
          -- expressions, to be evaluated when functions are inlined.
          check (Im.GenIntT) new_str = let val = Im.EStatic $ Im.UnOp Im.Bitsize new_str
                                       in  return (Im.IntT (Im.UnOp Im.Bitsize val) , val)

          check typ _              = throwErr cL (str <<
                                                  " is not a struct, it is of type " <<
                                                  typ)

          doStruct fields_info new_str = 
              do -- get just the [TypedName]
                 let (fields,_)    = fields_info
                 case lookup fieldname fields of
                          (Just t) -> do let field_idx = fromJust $
                                                         findIndex ((== fieldname) . fst) $
                                                         fields
                                         return (t, Im.EStruct new_str field_idx)
                          _        -> throwErr cL $ ("struct has no field " << fieldname)


checkExp e@(T.EStruct str _)
    = throwErr cL $ "struct field in " << e << " is not a simple name"


-- to check a function call:
-- - check and get types for all the args
-- - compare those to the function's formal params
-- TODO: not finished with the checking here! need to check that the
-- actual args in the call have the correct types
checkExp e@(T.EFunCall (T.Ident fcnName) args) =
    do (Im.Func _ _ t form_args _)      <- extractFunc fcnName
       im_args                          <- mapM (checkExp . extrExp) args
       form_types                       <- mapM (canonicalize . snd)          form_args
       act_types                        <- mapM (canonicalize . Im.getExpTyp) im_args
       let mismatch                     = findIndex (uncurry (/=) . mapTuple2 Im.stripRefQual)
                                                    (zip form_types act_types)
       -- if a mismatch found above, report the error.
       maybe (return ())
             (\i -> throwErr cL ("function call " << e
                                 << ": type error at param " << i
                                 << ", expected " << (form_types !! i)
                                 << ", got " << (act_types !! i)))
             mismatch
       return (Im.ExpT t (Im.EFunCall fcnName im_args))

    where extrExp (T.FunArg e) = e


checkExp e
    | Just (_, op, e1, e2) <- analyzeBinExp e
         = setContext e $
           do new_e1 <- checkExp e1
              new_e2 <- checkExp e2
              (Im.ExpT t new_e) <- case classifyBinExp e of
                                      Im.Arith      -> checkArith op new_e1 new_e2
                                      Im.Binary     -> checkBinary op new_e1 new_e2
                                      Im.Logical    -> checkLogical op new_e1 new_e2
                                      Im.Comparison -> checkComparison op new_e1 new_e2
              -- propagate EStatic if both params are static.
              let static_e = case (all isStaticExp [new_e1, new_e2]) of
                               True  -> Im.ExpT t (Im.EStatic new_e)
                               False -> Im.ExpT t             new_e
              return static_e

    | Just (op, arg_e) <- analyzeUnaryOp e   = setContext e $
                                               do new_arg_e <- checkExp arg_e
                                                  res_t     <- getUnaryOpTyp op new_arg_e
                                                  let new_e = (Im.ExpT res_t $ Im.UnOp op new_arg_e)
                                                  typ <- checkUnary new_e
                                                  return $ annot typ new_e

    where isStaticExp (Im.EStatic _)    = True
          isStaticExp _                 = False
          -- get the Im.Typ for an operator
          getUnaryOpTyp :: Im.UnOp -- the operator
                        -> Im.Exp -- the full Im.Exp for the param
                        -> StateWithErr Im.Typ
          getUnaryOpTyp Im.Not (Im.ExpT Im.BoolT _) = return $ Im.BoolT
          getUnaryOpTyp op     (Im.ExpT (Im.IntT i) _)
              | op /= Im.Not                        = return $ Im.IntT i
          getUnaryOpTyp op      e                   = throwErr cL ("Unary expression arg " << e
                                                                   << " has invalid type")



                                          
                 

checkTypedName :: T.TypedName -> StateWithErr (Im.Ident, Im.Typ)
checkTypedName (T.TypedName t (T.Ident name)) = do t_new <- checkTyp t
                                                   return (name, t_new)




-- make sure an LVal is actually assignable
checkLVal :: T.LVal -> StateWithErr Im.Exp

-- an identifier needs special treatment: don't assign to consts etc; only
-- assign to variables
checkLVal lv@(T.LVal (T.EIdent (T.Ident name))) =
    setContext lv $
    do ent <- extractEnt name
       case ent of
          (EntVar (t,v)) | not $ elem Im.Immutable (Im.vflags v) 
                  -> return (Im.ExpT t $ Im.EVar v)
          _
                  -> throwErr cL $ "Assigning to immutable identifier " << name

checkLVal (T.LVal e) =
    setContext e $
    -- here we do a checkLVal on the array and struct sub-expressions, to make sure they
    -- are themselves valid lvals, to catch stuff like
    -- const N = ...;
    -- N[i] = x;
    do im_e <- case e of
                   e@(T.EStruct e_str _) -> do checkLVal (T.LVal e_str)
                                               checkExp e
                   e@(T.EArr e_arr _)    -> do checkLVal (T.LVal e_arr)
                                               checkExp e
                   _                 -> throwErr cL $ e << " is not a legal lvalue"
       return im_e


-- x <?> ctx = 

{-
-- just to tinker with the (Haskell) types here.
foo :: Integer -> ErrMonad Integer
foo i
    | i < 10    = return 2
    | otherwise = throwError $ Err 47 "kuku"
-}


checkLoopCounter :: Im.Ident -> StateWithErr ()
checkLoopCounter name =
    -- if extractEnt returns an error, that's fine, we don't want to
    -- propagate it, hence we replace it with a dummy Entity value
    -- (EntConst 0)
    do var <- extractEnt name `catchError` (const $ return $ EntConst 0)
       case var of
         (EntVar (_,v))
             | elem Im.LoopCounter (Im.vflags v)  -> throwErr cL $ "Loop counter " << name
                                                                << " reused in nested loop"
         _                                        -> return ()
         


-- | canonicalize a type for the purpose of assignment
-- type mismatch checking, and function-call argument checking:
-- set all int sizes to be the same,
-- strip RefT's
-- substitute all top-level typedefs referring to simple scalar types (Int and
-- Bool), but leave typedefs referring to Structs and Arrays.
canonicalize :: Im.Typ -> StateWithErr Im.Typ
canonicalize t = (flip logDebug) ("canonicalize " << t) $
                 case t of (Im.IntT i)          -> return $ Im.IntT 32
                           (Im.RefT t')         -> canonicalize t'
                           _                    -> do t'    <- Im.expandType [Im.DoTypeDefs] t
                                                      case t' of
                                                        (Im.IntT _)      -> canonicalize t'
                                                        (Im.BoolT)       -> return t
                                                        _                -> return t



-- create initialization statements for local variables
-- needs to be in StateWithErr because it calls (lookupType)
mkVarInits :: Im.VarTable -> StateWithErr [Im.Stm]
mkVarInits local_table = do let varlist  = Map.toList local_table
                            concatMapM mkInit $ map (\(n, (t,v)) -> (n,t,Im.EVar v)) varlist
          -- create Init statements for an lval of the given type
    where mkInit :: (String, Im.Typ, Im.Exp) -> StateWithErr [Im.Stm]
          mkInit (name,t,lval)
              = case t of
                              -- NOTE: scalar default values defined here!
                              (Im.IntT i)          -> return [ass lval 0 t]
                              (Im.BoolT)           -> return [ass lval (Im.lbool False) t]
          -- for a struct, we first add an SAss for the whole struct,
          -- using Im.EStructInit,
          -- and then individual SAss for each member, recursively
                              (Im.StructT fields_info)    ->
                                  do let (fields, flocs)    = fields_info
                                         locs               = map Im.valloc flocs
                                         (_,lens)           = unzip locs
                                     inits <- zipWithM (mkStruct lval t)
                                                       fields
                                                       [0..]
                                     return $ (ass lval (Im.EStructInit (sum lens)) t) :
                                              concat inits
                              (Im.ArrayT elem_t len_e)    ->
                                   do elem_len   <- Im.expandType [Im.DoAll] elem_t >>== 
                                                    Im.typeLength Im.tblen
                                      len        <- Im.evalStatic len_e >>== fromInteger
                                      return [ass lval (Im.EArrayInit name elem_len len) t]
                              (Im.SimpleT tname)    ->
                                   do typ <- lookupType tname
                                      mkInit (name, typ, lval)

                              _                     -> return []
                      `trace` ("mkInit " << lval << "::" << t)
                            

          ass lval val t = Im.SAss (Im.ExpT t lval) (Im.ExpT t val)

          -- create a field-init statement for struct 'str', for the
          -- given field (ie. its type, offset and length)
          mkStruct str str_t (_, t) field_idx = mkInit ("", t, (Im.EStruct (Im.ExpT str_t str)
                                                                            field_idx))


-- | Here we always apply strShow to get the pretty context

-- type is same as EWC.setContext except for the StreamShow instance
setContext :: (StreamShow c, Error e, Monad m) =>
              c ->
              ErrorT (EWC.ErrorWithContext e String) m a ->
              ErrorT (EWC.ErrorWithContext e String) m a
setContext = EWC.setContext . strShow


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

          -- nothing to do for Exp.
          f_e                   = myLiftM id



-- at the global scope, the VarTable has only one level
atGlobalScope = do TCS {vars=vs} <- St.get
                   return (length vs == 1)


-- extractor of different table objects
data Entity = EntConst Integer
            | EntFunc  Im.Func
            | EntVar   (Im.Typ,Im.Var)
            | EntType  Im.Typ

extractEnt :: Im.EntName -> StateWithErr Entity
extractEnt     name = do TCS {types=ts,
                              consts=cs,
                              funcs=fs,
                              vars=vs} <- St.get
                         case extractHelper (ts,cs,fs,vs) name of
                           (Just ent)   -> return ent
                           _            -> throwErr cL $ "Identifier " << name
                                                           << " not in scope"



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


extractFunc     name = do TCS {funcs=fs} <- St.get
                          let res = maybeLookup name [fs]
                          case res of (Just f) -> return f
                                      _        -> throwErr cL $ name
                                                              << " is not in scope as a function"

-- check a unary opeartion, and return its type.
-- here 'e' is the whole unary expression, not just the parameter
checkUnary e@(Im.ExpT t inner_e) =
         do t_full <- checkExpExpand t
            case (inner_e, t_full) of
                         ( (Im.UnOp Im.Not _),     (Im.BoolT) )   -> return Im.BoolT
                         ( (Im.UnOp Im.BNot _), it@(Im.IntT _) )  -> return it
                         ( (Im.UnOp Im.Neg  _), it@(Im.IntT _) )  -> return it
                         ( _                  , _              )  ->
                             throwErr cL $ "Unary operation " << inner_e
                                          << " has invalid param type " << t_full
                                 


-- check a logical expression. quite easy
checkLogical op e1@(Im.ExpT t1 _) e2@(Im.ExpT t2 _) =
    do t1_full <- checkExpExpand t1
       t2_full <- checkExpExpand t2
       let exp_out = (Im.BinOp op e1 e2)
       case (t1_full, t2_full) of
                (Im.BoolT, Im.BoolT) -> return $ Im.ExpT Im.BoolT exp_out
                _                    -> throwErr cL $ "Logical expression " << exp_out
                                                   << " does not have Bool args"

-- check an arithmetic expression whose components e1 and e2 are already checked.
checkBinary op e1 e2 =
    do let ( (Im.ExpT t1 _), (Im.ExpT t2 _) ) = (e1,e2)
       t1_full <- checkExpExpand t1
       t2_full <- checkExpExpand t2
       -- set the type of the result expression - try to
       -- FIXME: need a more elegant way to remove the reference qualifier on arg types
       -- here
       case (Im.stripRefQual t1_full, Im.stripRefQual t2_full) of
             ((Im.IntT i1_e),
              (Im.IntT i2_e) )  -> do i_out <- Im.tryEvalStaticBin max Im.Max i1_e i2_e
                                      return $ annot (Im.IntT i_out) (Im.BinOp op e1 e2)
             _                  -> throwErr cL $ "Arithmetic expression "
                                            << (Im.BinOp op e1 e2)
                                            << " has non-integer params"

-- same as checkBinary except we have to increment the bitsize of the
-- output by 1
-- FIXME: how about with multiplication?? this seems like a dirty area, the bit size *may*
-- expand a lot, but probably wont, but how do we know?
-- in fact, expanding bit size when adding is dodgy too, could build up quickly
checkArith op e1 e2 = do (Im.ExpT (Im.IntT i) e) <- checkBinary op e1 e2
                         return $ Im.ExpT (Im.IntT (i+1)) e
{-
checkArith op e1 e2 =
    case (e1,e2) of
       ( (T.IntT i1) _),
         (T.ExpT (T.IntT i2) _) )    -> return ( (op e1 e2), (T.EMax i1 i2) )
       _                             -> throwErr cL $ "Arithmetic expression "
                                                      << (op e1 e2)
                                                      << " has non-integer params"
-}

-- the parameters must have the same type, modulo RefT, and can be Bool, Int or Enum
checkComparison op e1@(Im.ExpT t1 _) e2@(Im.ExpT t2 _) =
    do full_t1 <- checkExpExpand t1 >>== Im.stripRefQual
       full_t2 <- checkExpExpand t2 >>== Im.stripRefQual
       let answer = (Im.ExpT Im.BoolT (Im.BinOp op e1 e2))
       case (full_t1, full_t2) of
          (Im.BoolT,  Im.BoolT)                         -> return answer
          (Im.IntT _, Im.IntT _)                        -> return answer
          (Im.EnumT nm1 _, Im.EnumT nm2 _) | nm1 == nm2 -> return answer
          _                      -> throwErr cL $ "Args to comparison expression "
                                                  << answer
                                                  << " are of incompatible types "
                                                  << show (full_t1) << " and " << show(full_t2)

-- a type expand function for the check expression functions above
checkExpExpand = Im.expandType [Im.DoRefs, Im.DoTypeDefs]

mkVarSet :: Im.VarTable -> Im.VarSet
-- toAscList gives a sorted list of (key,value), and the values are
-- (typ,var), thus (snd . snd) to extract the var
mkVarSet = Cont.fromList . (map (snd . snd)) . Map.toAscList

----------------------------------------------------------
----------------------------------------------------------



-- push and pop a SymbolTable scope
pushScope = St.modify $ projToVars $ (push Map.empty)
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
    | otherwise                               = 
        case e of
               (T.EInt i)       -> return i
               (T.EIdent (T.Ident nm)) ->
                   do -- has to be a constant, look in the ConstTable
                     TCS {consts=ct} <- St.get
                     let val = maybeLookup nm [ct]
                     case val of
                              (Just i) -> return i
                              _        -> throwErr cL ("Supposedly static expression " << e
                                                       << " is not static: "
                                                       << nm << " is not a constant")
               _                -> throwErr cL $ "Invalid static expression " << e



addToFuncs :: Im.EntName -> Im.Func -> StateWithErr ()
addToFuncs name f = St.modify $
                    projToFuncs $
                    Map.insert name f

addToTypes :: Im.EntName -> Im.Typ -> StateWithErr ()
addToTypes name typ = St.modify $
                      projToTypes $
                      Map.insert name typ


-- make a Var from a variable name, with optional flags specified.
name2var flags name = let v' = (Im.VSimple name)
                      in
                        if null flags
                        then v'
                        else (Im.VFlagged flags v')


-- add a variable to the table.
-- takes care of constructing a VFlagged if there are flags
addToVars typ flags name = St.modify $
                           projToVars $
                           modtop $
                           Map.insert name (typ, name2var flags name)

-- | Modify the named variable with a function.
-- Error if it does not exist in the var tables.
modVar :: String -> (Im.Var -> Im.Var) -> StateWithErr ()
modVar name f               = do varmaps   <- St.gets vars
                                 -- if the update succeeds, insert the new varmaps,
                                 -- otherwise report error.
                                 maybe (throwErr cL $ "Variable " << name << " not declared")
                                       (\maps   -> St.modify $ projToVars $ const maps)
                                       (modify_first_map name (projSnd f) varmaps)
                                           


lookupType :: String -> StateWithErr Im.Typ
lookupType name =
     do TCS {types=ts} <- St.get
        let res = maybeLookup name [ts]
        case res of
           Just t -> return t
           _      -> throwErr cL $ "Type " << name << " is not in scope"


{-
getTypeTable = do TCS {types=ts} <- St.get
                  return ts
-}

-- getRvalTyp name     = lookupSym Var name


                      
-- annotate an expression
annot t e = Im.ExpT t e




-- also takes a predicate which is applied to the exp, and only matching exps are returned
analyzeSomeBinExps pred e = if pred e then analyzeBinExp e else Nothing

analyzeBinExp e = case e of
                     (T.EPlus e1 e2)      -> Just (Im.Arith,Im.Plus,e1,e2)
                     (T.EMinus e1 e2)     -> Just (Im.Arith,Im.Minus,e1,e2)

                     (T.ETimes e1 e2)     -> Just (Im.Arith,Im.Times,e1,e2)
                     (T.EDiv e1 e2)       -> Just (Im.Arith,Im.Div,e1,e2)
                     (T.EMod e1 e2)       -> Just (Im.Arith,Im.Mod,e1,e2)

                     (T.EEq e1 e2)      -> Just (Im.Comparison,Im.Eq,e1,e2)
                     (T.ENeq e1 e2)     -> Just (Im.Comparison,Im.Neq,e1,e2)
                     (T.ELt e1 e2)     -> Just (Im.Comparison,Im.Lt,e1,e2)
                     (T.EGt e1 e2)     -> Just (Im.Comparison,Im.Gt,e1,e2)
                     (T.EGtEq e1 e2)     -> Just (Im.Comparison,Im.GtEq,e1,e2)
                     (T.ELtEq e1 e2)     -> Just (Im.Comparison,Im.LtEq,e1,e2)

                     (T.EAnd e1 e2)    -> Just (Im.Logical,Im.And,e1,e2)
                     (T.EOr e1 e2)     -> Just (Im.Logical,Im.Or,e1,e2)

                     (T.ESL e1 e2)   -> Just (Im.Binary,Im.SL,e1,e2)
                     (T.ESR e1 e2)   -> Just (Im.Binary,Im.SR,e1,e2)
                     (T.EBOr e1 e2)  -> Just (Im.Binary,Im.BOr,e1,e2)
                     (T.EBAnd e1 e2) -> Just (Im.Binary,Im.BAnd,e1,e2)
                     (T.EBXor e1 e2) -> Just (Im.Binary,Im.BXor,e1,e2)

                     _                  -> Nothing

analyzeUnaryOp e = case e of
                    (T.ENeg e1)          -> Just (Im.Neg, e1)
                    (T.ENot e1)          -> Just (Im.Not, e1)
                    (T.EBNot e1)         -> Just (Im.BNot, e1)
                    _                   -> Nothing


classifyBinExp e = case analyzeBinExp e of
                                        Just (kind,_,_,_) -> kind
                                        _                 -> Im.NotBin

                     

-- some predicates
isArithOp = (== Im.Arith) . classifyBinExp
isCompOp = (== Im.Comparison) . classifyBinExp
isLogicOp = (== Im.Logical) . classifyBinExp



-- use a whole T.Exp to indicate the operator so we can reuse those values
doOp :: T.Exp -> Integer -> Integer -> Integer
doOp (T.EPlus _ _) = (+)
doOp (T.EMinus _ _) = (-)
doOp (T.ETimes _ _) = (*)


-- give quick StreamShow instances to the concrete syntax types which need it here, via
-- their Bnfc-provided Show instances
{-
instance StreamShow T.Stm   where strShows  = showsPrec 0
instance StreamShow T.Exp   where strShows  = showsPrec 0
instance StreamShow T.LVal  where strShows  = showsPrec 0
instance StreamShow T.Dec   where strShows  = showsPrec 0
instance StreamShow T.Typ   where strShows  = showsPrec 0
-}
instance StreamShow T.Stm   where strShow  = Print.printTree
instance StreamShow T.Exp   where strShow  = Print.printTree
instance StreamShow T.LVal  where strShow  = Print.printTree
instance StreamShow T.Dec   where strShow  = Print.printTree
instance StreamShow T.Typ   where strShow  = Print.printTree
#ifdef SYNTAX_C
instance StreamShow T.AssStm   where strShow  = Print.printTree
#endif

testDecs = [ T.ConstDecl (T.Ident "x") (T.EInt 10),
             T.TypeDecl  (T.Ident "y") T.BoolT ]
