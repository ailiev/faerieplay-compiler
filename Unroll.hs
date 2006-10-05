module Unroll where


-- import Debug.Trace

import Control.Monad.Error              (Error, noMsg, strMsg, throwError, ErrorT(..))
import Control.Monad.Identity           (Identity, runIdentity)
import Control.Monad.Writer             (Writer, runWriter, tell)

import Data.Bits
import qualified Control.Monad.State    as St
import Control.Monad.Trans              (lift)
import Maybe                            (fromJust)
import List                             (unfoldr, partition)
import qualified Data.Map               as Map

import SashoLib                         ((<<), ilog2, unfoldrM,
                                         mapTuple2)
import Stack                                    (Stack(..), maybeLookup)
import qualified Container              as Cont

import Common
import Intermediate
import HoistStms                        hiding (popScope, pushScope)

import qualified TypeChecker            as Tc





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
cMAXSCOPE = 128


-- type StateWithErr = St.StateT MyStateT ErrMonad
type StateWithErr = ErrorT MyErrorCtx (St.State MyStateT)


-- to throw an error inside the StateWithErr monad
throwErr :: Int -> String -> StateWithErr a
throwErr p msg = throwErrorCtx $ Err p msg


logError line msg = tell [Err line msg]





-- the full version, usign both State and Error
unrollProg :: Prog -> ErrCtxMonad [Stm]
unrollProg (Prog pname pt@(ProgTables {funcs=fs})) =
    let (Func _ _ t form_args stms) = fromJust $ Map.lookup "main" fs
        startScope                  = pushScope []
        startState                  = (pt, startScope)
        (val_or_err,state')         = St.runState (runErrorT $ mapM unroll stms) startState
    in case val_or_err of
                Left err        -> Left err
                Right stmss     -> Right (concat stmss)




-- scope invariants:
-- unroll is called with the correct scope depth, but with the top-level scope
-- number of the previous unroll call. Thus, have to increment the scope at the
-- start, and add a new depth before calling unroll recursively

-- :::::::::::::::::::::
unroll :: Stm -> StateWithErr [Stm]
-- :::::::::::::::::::::

-- unroll s@(SAss lv e@(EFunCall nm args)) = return [s]
{-
  Implementation of reference parameters:
  Replace all their usages in the body with the actual parameter
-}
unroll s@(SAss lv e@(EFunCall nm args)) = genericUnroll unrollAss s
    where unrollAss scope (SAss lv e@(EFunCall nm args)) =
              do (Func name locals t form_args stms) <- extractFunc nm
                 let                      -- pair up the formal vars and their values
                     arg_complex = zip form_args args

                     -- split up reference and non-ref args (paired with their values),
                     -- ditch the formal var type
                     -- annotations, and make var's from the formal names
                     (ref_args,
                      nonref_args)  = mapTuple2 (map (\((nm,t),val) -> (formarg2var scope nm , val))) $
                                      partition (isRefType . snd . fst) $
                                      arg_complex

                     -- prepare the statements
                     stms'      = removeRefQuals .
                                  replaceRefs ref_args .
                                  addLocalScope scope locals $
                                  stms

                     -- assignments to the non-ref formal params from their values
                     ass's      = [SAss (EVar formarg)
                                        actual_arg      | (formarg,actual_arg) <- nonref_args]

                     -- assignment to lval from the function return parameter
                     retass     = [SAss lv (fcnRetVar nm scope)]

                 return $ ass's ++ stms' ++ retass

          -- NOTE: this is how formal param vars in the function body are
          -- marked in TypeChecker.hs, except there is no scope to deal with
          -- there. Now, want to re-generate those vars in order to assign them
          -- the actual param values, or replace them in the case of reference
          -- args.
          formarg2var scope name = addScope scope $ Tc.name2var [FormalParam] name

          -- add the local scope to *all* local var occurances
          addLocalScope scope locals stms  = map (scopeVars locals scope) stms
                                                 `trace`
                                                 ("unroll func: name = " << nm <<
                                                  "; locals = " << locals <<
                                                  "; scope = " << scope)
          -- replace reference local vars with their referrents
          -- remove the ExpT on the value before subbing it in, as there should already be
          -- an equivalent ExpT around the var
          replaceRefs ref_args  = map (\stm -> foldl (\s (var,val) -> let (ExpT _ val_e) = val
                                                                      in
                                                                        subst var val_e s)
                                                     stm
                                                     ref_args)
          -- remove RefT's in ExpT annotations (as the actual refs are already removed by
          -- replaceRefs)
          removeRefQuals        = map stmStripRefQual




unroll s@(SBlock _ _) = genericUnroll unrollBlock s
    where unrollBlock scope (SBlock locals stms) =
              -- replace all local variables with Scoped ones, in stms
              return $ map (scopeVars locals scope) stms


unroll s@(SFor _ _ _ _) = genericUnroll unrollFor s
    where unrollFor :: Scope -> Stm -> StateWithErr [Stm]
          unrollFor scope (SFor countVar begin_exp end_exp stms) =
              do begin  <- evalStatic begin_exp
                 end    <- evalStatic end_exp
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
                     -- References to the loop counter will have been scoped, so scope
                     -- countVar so it's the same.
                     countVar' = addScope scope countVar
                     -- can have the loop count forwards and backwards by 1
                     countVals | begin <= end   = [begin..end]
                               | otherwise      = reverse [end..begin]
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




genericUnroll :: (Scope -> Stm -> StateWithErr [Stm]) -> Stm -> StateWithErr [Stm]
genericUnroll f stm = do St.modify incrScope'
                         scope <- St.gets getsScope
                         checkScopeDepth scope
                         -- do the "real work"
                         stms <- f scope stm
                         -- and recurse
                         St.modify pushScope'
                         stmss <- mapM unroll stms
                         St.modify popScope'
                         return $ concat stmss
                         

-- checkScopeDepth :: Scope -> ErrCtxMonad ()
checkScopeDepth scope
    | length scope > cMAXSCOPE = throwErrorCtx $
                                 Err 42 $ ("while unrolling: Function/Block recursion deeper than "
                                           << cMAXSCOPE)
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
                            _           -> throwErr 42 $ "Unroll.extractFunc "
                                                          << name << " failed!"




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


-- remove all RefT's in an Stm
stmStripRefQual = mapStm id f_e
    where f_e = mapExp f
          -- remove RefT's in an Exp
          f (ExpT (RefT t) e)   = (ExpT t e)
          f e                   = e


------------------
-- Scope utilities
------------------

-- enter a new scope depth (eg. upon entering a function call)
-- uses the Stack class functions
pushScope   :: Scope -> Scope
pushScope   = push 0
popScope    :: Scope -> Scope
popScope    = pop


-- enter the next scope at the same depth (eg. from one function call to the
-- next)
incrScope = modtop (+1)



testExp = (BinOp Plus (var "x") (ELit $ LInt 5))
