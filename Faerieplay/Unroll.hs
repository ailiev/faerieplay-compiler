{-# OPTIONS_GHC -fglasgow-exts #-}
-- -fglasgow-exts for the type constraint of inScope

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


module Faerieplay.Unroll where


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

import Faerieplay.SashoLib              ((<<), ilog2, unfoldrM,
                                         mapTuple2, iterateWhileM,
                                         fromJustMsg)
import Faerieplay.Stack                 (Stack(..), maybeLookup)
import qualified Faerieplay.Container   as Cont

import Faerieplay.Common
import Faerieplay.Intermediate
import Faerieplay.HoistStms

import qualified Faerieplay.TypeChecker as Tc





type MyStateT = (ProgTables , Scope)
-- and accessor functions:
applyToPT :: (ProgTables -> ProgTables) -> MyStateT -> MyStateT

applyToPT f (pt,scope) = (f pt, scope)
applyToScope f (pt,scope) = (pt, f scope)

pushScope' = applyToScope pushScope
incrScope' = applyToScope incrScope
popScope'  = applyToScope popScope

getPT = fst
getScope = snd


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
    let (Func _ _ t form_args stms) = fromJustMsg ("failed to find \"" ++ cMAINNAME
                                                   ++ "\" function") $
                                      Map.lookup cMAINNAME fs
        startScope                  = pushScope []
        startState                  = (pt, startScope)
        (val_or_err,state')         = St.runState (runErrorT $ mapM unroll stms)
                                                  startState
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
                      nonref_args)  = mapTuple2 (map (\((nm,t),val) ->
                                                          (formarg2var scope nm , val))
                                                ) $
                                      partition (isRefType . snd . fst) $
                                      arg_complex

                     -- prepare the statements
                     stms'      = removeRefQuals .
                                  replaceRefs ref_args .
                                  addLocalScope scope locals $
                                  stms

                     -- assignments to the non-ref formal params from their values
                     ass's      = [SAss (ExpT t (EVar formarg))
                                        actarg
                                   | (formarg,
                                      actarg@(ExpT t _)) <- nonref_args]

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


unroll s@(SFor _ _ _) = genericUnroll unrollFor s
    where unrollFor :: Scope -> Stm -> StateWithErr [Stm]
          unrollFor scope (SFor countVar countVals stms) =
              do -- the only new local variable in a for-loop body is
                 -- the counter variable, so create a VarSet
                 -- containing just that
                 let stms' = map (scopeVars (Cont.singleton (stripScope countVar))
                                            scope)
                                 stms
                     stmss = replicate (length countVals) stms'
                     --
                     -- and subst correct counter values into all the statements
                     --
                     -- References to the loop counter will have been scoped, so scope
                     -- countVar so it's the same.
                     countVar' = addScope scope countVar
                     -- a version of subst for each unrolled block of stm's
                     substs = [subst countVar' (lint val) | val <- countVals]
                     -- each 'stms' list is mapped a subst with the correct counter value
                     stmss' = zipWith map substs stmss
                 return $ concat stmss'

{-
unroll s@(SFor_C _ _ _ _ _) = genericUnroll unrollFor_C s
    where unrollFor_C scope (SFor_C countVar
                                    begin_exp
                                    stop_cond
                                    update_ass
                                    stms)           =
              do countVals  <- getCountVals countVar begin_exp stop_cond update_ass
                               `logDebug`
                               ("unroll(SFor_C): stop_cond = " << show stop_cond
                                << ", update_ass = " << show update_ass)
                 error ("Got the vals: " ++ show countVals)

          getCountVals countVar begin_exp stop_cond update =
              do begin      <- evalStatic begin_exp
                 vals       <- iterateWhileM (keepgoing countVar stop_cond)
                                             (nextVal countVar update)
                                             begin
                 return vals
                 

          nextVal countVar (AssStm lval op rval) x =
              do let rval'  = substExp countVar (lint x) rval
                 -- FIXME: have to use 'op' here, not just a straight assignment
                 lval'      <- evalStatic rval'
                 return lval'

          keepgoing countVar stop_cond x           =
              do let cond   = substExp countVar (lint x) stop_cond
                 cond_val   <- evalStatic cond
                 return (toEnum $ fromIntegral cond_val)
                            `logDebug`
                            ("keepgoing (x=" << x << "): cond_val = " << cond_val)
-}


unroll s@(SIfElse test (locs1,stms1) (locs2,stms2)) =
    do stmss1'  <- mapM unroll stms1
       stmss2'  <- mapM unroll stms2
       return [SIfElse test (locs1, concat stmss1') (locs2, concat stmss2')]

-- simple leftovers: SAss without an EFunCall on the right, and SPrint
unroll s            = return [s]




genericUnroll :: (Scope -> Stm -> StateWithErr [Stm]) -> Stm -> StateWithErr [Stm]
genericUnroll f stm = do St.modify incrScope'
                         scope <- St.gets getScope
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
          f_s (SFor counter ctrvals stms)       = SFor (addScope scope counter)
                                                       ctrvals
                                                       stms
          f_s s                                 = s

          inScope :: (Cont.Container c Var) => c -> Var -> Bool
          inScope locals var = Cont.member (stripScope var) locals 

                               


-- to reconstruct the return variable of a function in a given scope
-- FIXME: rather awkward, may be better if a Func carried its return var around
-- explicitly
fcnRetVar fname scope = EVar (VScoped scope (VFlagged [RetVar] (VSimple fname)))


extractFunc name = do ProgTables {funcs=fs} <- St.gets getPT
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
