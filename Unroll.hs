module Unroll where


import Debug.Trace

import Control.Monad.Error (Error, noMsg, strMsg, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Writer (Writer, runWriter, tell)

import Data.Bits
import qualified Control.Monad.State as St
import Control.Monad.Trans (lift)
import Maybe (fromJust)
import List  (unfoldr)
import qualified Data.Map as Map

import SashoLib (maybeLookup, (<<), ilog2, unfoldrM, strictList)
import qualified Container as Cont

import Intermediate
import HoistStms hiding (popScope, pushScope)

import qualified TypeChecker as Tc



-- This is the type of our type error representation.
data MyError = Err {line::Int, reason::String} deriving (Show)

-- We make it an instance of the Error class
instance Error MyError where
  noMsg    = Err 0 "Type error"
  strMsg s = Err 0 s

type ErrMonad = Either MyError



type MyStateT = (ProgTables , Scope)
-- and accessor functions:
applyToPT :: (ProgTables -> ProgTables) -> MyStateT -> MyStateT

applyToPT f (pt,scope) = (f pt, scope)
applyToScope f (pt,scope) = (pt, f scope)

pushScope' = applyToScope pushScope
incrScope' = applyToScope incrScope
popScope'  = applyToScope popScope

getsPT = fst
getsScope = snd


-- maximum function recursion depth
cMAXSCOPE = 32


type StateWithErr = St.StateT MyStateT ErrMonad


-- to throw an error inside the StateWithErr monad
throwErr :: Int -> String -> StateWithErr a
throwErr p msg = lift $ throwError $ Err p msg


logError line msg = tell [Err line msg]





-- the full version, usign both State and Error
unrollProg :: Prog -> ErrMonad [Stm]
unrollProg (Prog pname pt@(ProgTables {funcs=fs})) =
    let (Func _ _ t form_args stms) = fromJust $ Map.lookup "main" fs
        startScope = pushScope []
        out = St.runStateT (mapM unroll stms) (pt, startScope)
    in case out of
                Left err        -> Left err
                Right (stmss,_) -> Right (concat stmss)




-- scope invariants:
-- unroll is called with the correct scope depth, but with the top-level scope
-- number of the previous unroll call. Thus, have to increment the scope at the
-- start, and add a new depth before calling unroll recursively

-- :::::::::::::::::::::
unroll :: Stm -> StateWithErr [Stm]
-- :::::::::::::::::::::

-- unroll s@(SAss lv e@(EFunCall nm args)) = return [s]
unroll s@(SAss lv e@(EFunCall nm args)) = genericUnroll unrollAss s
    where unrollAss scope (SAss lv e@(EFunCall nm args)) =
              do (Func name locals t form_args stms) <- extractFunc nm
                 -- replace all local variables with Scoped ones, in stms
                 let stms'      = map (scopeVars locals scope) stms
                     form_args' = map (addScope scope) form_args
                 -- substitute actual values for all the formal params, in all
                 -- stms
                 -- substs is a list of subst partial applications:
                 -- substs :: [Stm -> Stm]
                     substs     = [subst farg arg | farg <- form_args', arg <- args]
                     stms''     = map (\stm -> foldl (flip ($)) stm substs) stms'
                     -- append an assignment to the target var
                     stms3  = stms'' ++ [SAss lv (fcnRetVar nm scope)]
                 return stms3



unroll s@(SBlock _ _) = genericUnroll unrollBlock s
    where unrollBlock scope (SBlock locals stms) =
              do -- replace all local variables with Scoped ones, in stms
                 let stms' = map (scopeVars locals scope) stms
                 return stms'


unroll s@(SFor _ _ _ _) = genericUnroll unrollFor s
    where unrollFor scope (SFor countVar lo_exp hi_exp stms) =
              do lo <- lift $ evalStatic lo_exp
                 hi <- lift $ evalStatic hi_exp
                 let stms' = map (scopeVars (Cont.singleton (stripScope countVar))
                                            scope)
                                 stms
--                 return stms'
                     -- the actual unrolling!
--                     dummy = trace ("num reps = " << (hi - lo)) 3
                     stmss = replicate (fromInteger (hi-lo)) stms'
                     -- and subst correct counter values into all the statements
                     -- a version of subst for each unrolled block of stm's
                     countVar' = addScope scope countVar
                     substs = [subst countVar' (ELit (LInt val)) | val <- [lo..hi]]
                     stmss' = zipWith map substs stmss
                 return $ concat stmss'


unroll s@(SIf test stms) = do stmss' <- mapM unroll stms
                              return [SIf test (concat stmss')]


-- this will only be used for SAss without an EFunCall on the right                   
unroll s = return [s]



genericUnroll f stm = do St.modify incrScope'
                         scope <- St.gets getsScope
                         lift $ checkScopeDepth scope
                         -- do the "real work"
                         stms <- f scope stm
                         -- and recurse
                         St.modify pushScope'
                         stmss <- mapM unroll stms
                         St.modify popScope'
                         return $ concat stmss
                         


checkScopeDepth :: Scope -> ErrMonad ()
checkScopeDepth scope
    | length scope > cMAXSCOPE = Left $ Err 42 $ "Function recursion deeper than"
                                                 << cMAXSCOPE
    | otherwise                = return ()






-- set the scope of variables which are local in this block, as indicated by the
-- set 'locals'
scopeVars locals scope s = mapStm f_s f_e s
                           
    where f_e (EVar v)
              | inScope locals v                = EVar (addScope scope v)
          f_e e                                 = e

          -- also set the scope of the counter variable inside its For loop
          f_s (SFor counter lo hi stms)         = SFor (addScope scope counter)
                                                       lo
                                                       hi
                                                       stms
          f_s s                                 = s

          inScope :: VarSet -> Var -> Bool
          inScope locals var = Cont.member (stripScope var) locals 

                               

-- remove the scope from a Var
stripScope (VScoped sc v)             = v
stripScope v                          = v

-- if already scoped, just overwrite the old scope
addScope sc (VScoped scope_old v)     = (VScoped sc v)
addScope sc v                         = (VScoped sc v)
               

-- to reconstruct the return variable of a function in a given scope
-- FIXME: rather awkward, may be better if a Func carried its return var around
-- explicitly
fcnRetVar fname scope = EVar (VScoped scope (VFlagged [RetVar] (VSimple fname)))


extractFunc name = do ProgTables {funcs=fs} <- St.gets getsPT
                      case maybeLookup name [fs] of
                            (Just f)    -> return f
                            _           -> throwErr 42 $ "Unroll.extractFunc failed!"



evalStatic :: Exp -> ErrMonad Integer
evalStatic e = case e of
                      (ELit l)          -> evalLit l
                      (ExpT _ e)        -> evalStatic e
                      (EStatic e)       -> evalStatic e
                      _                 -> throwError $ Err 42 "Not static!"


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


transIntOp op = case op of
                        Plus    -> (+)
                        Minus   -> (-)
                        Times   -> (*)
                        BAnd    -> (.&.)
                        BOr     -> (.|.)
                        Eq      -> (==)
                        Gt     -> (>)
                        Lt     -> (<)
                        GtEq   -> (>=)
                        Max     -> max

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
                         


-}


--unrollStms =  unrollFor




-- substitute a value for a variable into a statement
-- The only actaul substitution is at EVar expressions, this here just
-- needs to recurse the computation through the statement (using mapStm)
subst :: Var -> Exp -> Stm -> Stm
subst var val s = mapStm f_s f_e s
    where f_e   = substExp var val
          f_s   = id


-- substitue a value for a var into an exp
--          var      val    exp    result
substExp :: Var -> Exp -> Exp -> Exp
substExp var val exp = mapExp f exp
    where f (EVar var2) | var2 == var   = val
          f e                           = e


testExp = (BinOp Plus (var "x") (ELit $ LInt 5))
