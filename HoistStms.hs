-- code to hoist statements out of expressions.
-- in this case we hoist just function calls (which can have
-- side-effects)

-- NOTABENE: This is also where we convert SIfElse into two SIf:
-- if (p) s1 else s2
-- becomes:
-- temp = p;
-- if (temp) then s1;
-- if (!temp) then s2;

module HoistStms where

import qualified Control.Monad.State as St
import qualified Data.Map as Map

import SashoLib (Stack, push, pop, peek)
import qualified Container as Cont

import Intermediate hiding (pushScope, popScope)


-- the state for these computations:
-- 1) counter (Int) for naming temporary vars
-- 2) A (Stack VarSet) (implemented as [VarSet]) to gather up new temp
--    variables for every scope


type MyState = (Int,[VarSet])

getsInt = fst
getsVarSet = snd

modifyInt f (i,vs) = (f i, vs)
modifyVarSet f (i, vs) = (i, f vs)
 



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
      (SBlock vars stms)-> do pushScope
                              stmss <- mapM flatten stms
                              varset <- popScope
                              return $ [SBlock (Cont.union varset vars)
                                               (concat stmss)]
      (SFor id lo hi ss)-> do (stms_lo,lo_new) <- extrStms lo
                              (stms_hi,hi_new) <- extrStms hi
                              forss <- mapM flatten ss
                              return $ stms_lo ++ stms_hi ++ [(SFor id
                                                                    lo_new
                                                                    hi_new
                                                                    (concat forss))]
      (SIf test ss)     -> do (test_stms,test_new) <- extrStms test
                              ss_new <- mapM flatten ss
                              return $ test_stms ++ [(SIf test_new (concat ss_new))]
      (SIfElse t s1s s2s)->do (t_stms,t_new) <- extrStms t
                              s2ss_new <- mapM flatten s2s
                              s1ss_new <- mapM flatten s1s
                              i <- nextInt
                              let t_i = (tempVar i)
                              return $ t_stms ++
                                       [(SAss t_i t_new),
                                        (SIf t_i (concat s1ss_new)),
                                        (SIf (UnOp Not t_i) (concat s2ss_new))]

    where extrStms :: Exp -> St.State MyState ([Stm],Exp)
          extrStms e = do e_subbed <- subFunCalls e
                          let e_h = hoist e_subbed
                          return $ splitEseq e_h


{-
f_e   = hoist . subFunCalls

f_s s = case s of
        (SAss lval val)         -> do let (stms,val_new) = splitEseq val
                                      return $ smts ++ [(SAss lval val_new)]
        (SFor id lo hi s)       -> do 
-}


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
    where f (EFunCall nm args) = do sub_args   <- mapM subFunCalls args
                                    i          <- nextInt
                                    let t_i    =  tempVar i
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


pushScope =    St.modify $ modifyVarSet $ (`push` Cont.empty)
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
combHoisted e
    | P1 cons e1      <- classe = combHoistedL (\[e] -> cons e)         [e1]
    | P2 cons (e1,e2) <- classe = combHoistedL (\[e1,e2] -> cons e1 e2) [e1,e2]
    | PList cons es   <- classe = combHoistedL cons es
    | P0              <- classe = e
    where classe = classifyExp e
