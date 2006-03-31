module Unroll where


-- import Debug.Trace

import Control.Monad.Error (Error, noMsg, strMsg, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Writer (Writer, runWriter, tell)

import Data.Bits
import qualified Control.Monad.State as St
import Control.Monad.Trans (lift)
import Maybe (fromJust)
import List  (unfoldr)
import qualified Data.Map as Map

import SashoLib (maybeLookup, (<<), ilog2, unfoldrM, Stack(..))
import qualified Container as Cont

import Common
import Intermediate
import HoistStms hiding (popScope, pushScope)

import qualified TypeChecker as Tc





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
                 let stms'      = map (scopeVars locals scope) stms
                                        `trace`
                                        ("unroll func: name = " << nm <<
                                         "; locals = " << locals <<
                                         "; scope = " << scope)
                     -- slight HACK: recreating vars from the formal param list like this
                     -- is not well structured, but for now needed to get the formal arg
                     -- to look like its usage inside the function.
                     form_vars  = map (\(name,typ) -> addScope scope $
                                                      add_vflags [FormalParam] $
                                                      VSimple name)
                                      form_args
                     -- assignments to the formal params
                     ass's      = [SAss (EVar formarg)
                                        actual_arg      | formarg    <- form_vars
                                                        | actual_arg <- args]
                     -- assignment to lval from the function return parameter
                     retass     = [SAss lv (fcnRetVar nm scope)]
                 return $ ass's ++ stms' ++ retass




unroll s@(SBlock _ _) = genericUnroll unrollBlock s
    where unrollBlock scope (SBlock locals stms) =
              -- replace all local variables with Scoped ones, in stms
              return $ map (scopeVars locals scope) stms


unroll s@(SFor _ _ _ _) = genericUnroll unrollFor s
    where unrollFor scope (SFor countVar begin_exp end_exp stms) =
              do begin  <- lift $ evalStatic begin_exp
                 end    <- lift $ evalStatic end_exp
                 -- the only new local variable in a for-loop body is
                 -- the counter variable, so create a VarSet
                 -- containing just that
                 let stms' = map (scopeVars (Cont.singleton (stripScope countVar))
                                            scope)
                                 stms
                     -- have +1 here as the loop includes both end values
                     stmss = replicate (fromInteger (abs(begin-end) + 1)) stms'
                     --
                     -- and subst correct counter values into all the statements
                     --
                     countVar' = addScope scope countVar
                     -- can have the loop count forwards and backwards by 1
                     countVals | begin <= end   = [begin..end]
                               | otherwise      = [end..begin]
                     -- a version of subst for each unrolled block of stm's
                     substs = [subst countVar' (lint val) | val <- countVals]
                     -- each 'stms' list is mapped a subst with the correct counter value
                     stmss' = zipWith map substs stmss
                 return $ concat stmss'


unroll s@(SIfElse test (locs1,stms1) (locs2,stms2)) =
    do stmss1'  <- mapM unroll stms1
       stmss2'  <- mapM unroll stms2
       return [SIfElse test (locs1, concat stmss1') (locs2, concat stmss2')]

-- simple leftovers: SAss without an EFunCall on the right, and SPrint
unroll s            = return [s]





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
scopeVars :: VarSet -> Scope -> Stm -> Stm
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

                               


-- to reconstruct the return variable of a function in a given scope
-- FIXME: rather awkward, may be better if a Func carried its return var around
-- explicitly
fcnRetVar fname scope = EVar (VScoped scope (VFlagged [RetVar] (VSimple fname)))


extractFunc name = do ProgTables {funcs=fs} <- St.gets getsPT
                      case maybeLookup name [fs] of
                            (Just f)    -> return f
                            _           -> throwErr 42 $ "Unroll.extractFunc failed!"




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
substExp var val exp = mapExp f exp   -- `trace` ("try substExp " << var << " for " <<  val)
    where f (EVar var2) | var2 == var   = val    -- `trace` ("substExp " << var2 << " -> " << val)
          f e                           = e


------------------
-- Scope utilities
------------------

-- enter a new scope depth (eg. upon entering a function call)
-- uses the Stack class functions
pushScope :: Scope -> Scope
pushScope = push 0
popScope  :: Scope -> Scope
popScope        = pop


-- enter the next scope at the same depth (eg. from one function call to the
-- next)
incrScope = modtop (+1)



testExp = (BinOp Plus (var "x") (ELit $ LInt 5))
