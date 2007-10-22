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


-- code to hoist statements out of expressions.
-- in this case we hoist just function calls (which can have
-- side-effects)


-- Post Conditions:
-- have only if-else conditionals with simple-variable conditions, ie.
--                        no complex expressions
-- all function calls are alone on the right side of a SAss
-- there are a lot of new temporary variables :)
-- TODO: how "local" are these temp vars?? Do they ever span a conditional? Should
-- figure out and specify here.


-- Operations:
-- 
-- convert
-- if (p_exp) s
-- to
-- temp = p_exp
-- if (temp) s
-- so that conditional tests are always simple variables
--
-- convert SIf into an SIfElse with an empty else-clause


module Faerieplay.HoistStms (flattenProg,
                  flattenFunc   -- for testing in sfdlc.hs apparently.
                 ) where

import qualified Control.Monad.State as St
import qualified Data.Map as Map

import Faerieplay.SashoLib (mapTuple2, (>>==), replicateM)
import qualified Faerieplay.Container           as Cont
import Faerieplay.Stack                         (Stack(..))


import Faerieplay.Intermediate


-- the state for these computations:
-- 1) counter (Int) for naming temporary vars
-- 2) A (Stack VarSet) (implemented as [VarSet]) to gather up new temp
--    variables for every scope


type MyState = (Int,[VarSet])

getsInt = fst
getsVarSet = snd

modifyInt f (i,vs) = (f i, vs)
modifyVarSet f (i, vs) = (i, f vs)


-- | add a variable to the current scope's (ie. Func or deepest SBlock) var set
-- param is an EVar, or (ExpT (EVar v))
addVar :: Exp -> St.State MyState ()
addVar = St.modify . modifyVarSet . modtop . Cont.insert . extrVar
    where extrVar evar = let (EVar v)  = stripExpT evar
                         in  v



flattenProg :: Prog -> Prog
flattenProg (Prog pname pt@(ProgTables {funcs=fs})) =
    let new_fs = Map.map flattenFunc fs
    in Prog pname (pt {funcs=new_fs})


flattenFunc :: Func -> Func
flattenFunc f = fst $ St.runState (stateComp f) startState
    where stateComp (Func name vars t args stms) =
              do pushScope
                 stmss <- mapM flatten stms
                 varset <- popScope
                 return (Func name
                              (Cont.union vars varset)
                              t
                              args
                              (concat stmss))
          startState = (firstInt, [])



-- flatten: take a statement which has ESeq expressions, and pull all
-- the embedded statments out in front, producing a list of statements
-- without any ESeq anywhere
flatten :: Stm -> St.State MyState [Stm]


flatten s =
    case s of
      (SAss lval val)   -> do (stms,val_new) <- extrStms val
                              return $ stms ++ [(SAss lval val_new)]

      -- here we generate vars for all the vals, assign each var the right val, and use
      -- the generated vars in the resulting SPrint node, then assign back to the printed
      -- expressions, to make sure the print gate is fully in the data flow of the printed
      -- expressions.
      (SPrint p vals)   -> do (stmss,vals_new)  <- mapM extrStms vals >>== unzip
                              is                <- replicateM (length vals) nextInt
                              let t_is          = zipWith tempVarForExp vals_new is
                                  ass's         = zipWith SAss t_is vals_new
                                  reass's       = zipWith SAss vals_new t_is
                              mapM addVar t_is
                              return $ concat stmss ++
                                       ass's ++
                                       [(SPrint p t_is)] ++
                                       reass's

      (SBlock vars stms)-> do pushScope
                              stmss <- mapM flatten stms
                              varset <- popScope
                              return $ [SBlock (Cont.union varset vars)
                                               (concat stmss)]

      (SFor id ctrvals ss) ->
                           do forss <- mapM flatten ss
                              return $ [(SFor id
                                              ctrvals
                                              (concat forss))]
{-
      (SFor_C id lo stop update@(AssStm lval op rval) ss) ->
                           do (stms_lo,lo_new)  <- extrStms lo
                              (stms_st,st_new)  <- extrStms stop
                              -- FIXME: the rval here is actually quite limited, and
                              -- should not need extrStms
                              -- (stms_rval,rval_new)  <- extrStms lval
                              forss <- mapM flatten ss
                              return $ stms_lo ++ stms_st ++
                                       [SFor_C id
                                               lo_new
                                               st_new
                                               update
                                               (concat forss)]
-}

      (SIfElse t (locs1,s1s)
                 (locs2,s2s))  -> do (t_stms,t_new) <- extrStms t
                                     s2ss_new  <- mapM flatten s2s
                                     s1ss_new  <- mapM flatten s1s
                                     i         <- nextInt
                                     let t_i = tempVarForExp t i
                                     addVar t_i
                                     return $ t_stms ++
                                                [SAss t_i t_new,
                                                 SIfElse t_i (locs1, concat s1ss_new)
                                                             (locs2, concat s2ss_new)]

    where extrStms :: Exp -> St.State MyState ([Stm],Exp)
          extrStms e = do e_subbed <- subFunCalls e
                          let e_h = hoist e_subbed
                          return $ splitEseq e_h



-- hoist: take an expression (with component ESeq's), and produce one
-- with ESeq only at the top level
hoist = mapExp f
-- for ESeq we may have top-level ESeq's inside the inner expression,
-- thus get them out with splitEseq
-- ALSO: ESeq's in the 'stms' need to be cleaned up, ie. all
-- assignments brought directly into the 'stms' list, using flatAss
    where f e@(ESeq stms e1)    = let (ss,e_pure) = splitEseq e1
                                      stms_h      = concatMap flatAss stms
                                  in  (ESeq (stms_h++ss) e_pure)
-- all other Exp's just call combHoisted on the recursive
-- result, to move ESeq's to the top level
          f e                   = combHoisted e
         


-- replace all function calls in 'e' with an ESeq which assigns the
-- call to a temp var, and then returns the temp var
subFunCalls e = mapExpM f e
    where f f_e@(ExpT _ (EFunCall nm args)) =
                                 do sub_args   <- mapM subFunCalls args
                                    i          <- nextInt
                                    let t_i    =  tempVarForExp f_e i
                                    addVar t_i
                                    return $ ESeq [(SAss t_i (EFunCall nm sub_args))]
                                                  t_i
          f e                  = return e



-- bring all statements (ie Assignments) in ESeq expressions in the
-- 'val' to the outside, so there are no ESeq's left
flatAss :: Stm -> [Stm]
flatAss s@(SAss lval val) = let val_h = hoist val
                            in  case val_h of
                                     (ESeq stms e) -> stms ++ [(SAss lval e)]
                                     _             -> [s]



splitEseq :: Exp -> ([Stm],Exp)
splitEseq (ESeq stms e)   = (stms,e)
splitEseq e               = ([],  e)


firstInt = 0

-- return the current Int state, and increment the Int state
nextInt :: St.State MyState Int
nextInt = do i <- St.gets getsInt
             St.modify (modifyInt (+1))
             return i


pushScope =    St.modify $ modifyVarSet $ (push Cont.empty)
popScope  = do top <- St.gets (peek . getsVarSet)
               St.modify $ modifyVarSet $ pop
               return top


-- moves ESeq's from one level deep to just top-level
-- 'cons' is a function which makes an Exp from a list of other Exp's,
-- component_es are the potential ESeq's
combHoistedL :: ([Exp] -> Exp) -> [Exp] -> Exp
combHoistedL cons component_es = let (stmss, new_es) = unzip $ map splitEseq component_es
                                     stms = concat stmss
                                 in if   null stms
                                    then cons component_es
                                    else (ESeq stms (cons new_es))

combHoisted :: Exp -> Exp
combHoisted e = case classifyExp e of
                  P1 cons e1      -> combHoistedL (\[e] -> cons e)         [e1]
                  P2 cons (e1,e2) -> combHoistedL (\[e1,e2] -> cons e1 e2) [e1,e2]
                  PList cons es   -> combHoistedL cons es
                  P0              -> e
