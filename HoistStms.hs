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

import Intermediate

-- the state for these computations:
-- 1) counter (Int) for naming temporary vars
--


flattenFunc :: Func -> Func
flattenFunc f = fst $ St.runState (stateComp f) 0
    where stateComp (Func t vars stms) =
              do stmss <- mapM flatten stms
                 return (Func t vars (concat stmss))





-- flatten: take a statement which has ESeq expressions, and pull all
-- the embedded statments out in front, producing a list of statements
-- without any ESeq anywhere
flatten :: Stm -> St.State Int [Stm]


flatten s =
    case s of
      (SAss lval val)   -> do (stms,val_new) <- extrStms val
                              return $ stms ++ [(SAss lval val_new)]
      (SBlock stms)     -> do stmss <- mapM flatten stms
                              return $ concat stmss
      (SFor id lo hi s) -> do (stms_lo,lo_new) <- extrStms lo
                              (stms_hi,hi_new) <- extrStms hi
                              fors <- flatten s
                              return $ stms_lo ++ stms_hi ++ [(SFor id
                                                                    lo_new
                                                                    hi_new
                                                                    (groupStms fors))]
      (SIf test s)      -> do (test_stms,test_new) <- extrStms test
                              ss_new <- flatten s
                              return $ test_stms ++ [(SIf test_new (groupStms ss_new))]
      (SIfElse t s1 s2) -> do (t_stms,t_new) <- extrStms t
                              s2s_new <- flatten s2
                              s1s_new <- flatten s1
                              i <- nextInt
                              let t_i = (tempVar i)
                              return $ t_stms ++
                                       [(SAss (LVal t_i) t_new),
                                        (SIf t_i (groupStms s1s_new)),
                                        (SIf (UnOp Not t_i) (groupStms s2s_new))]

    where extrStms :: Exp -> St.State Int ([Stm],Exp)
          extrStms e = do e_subbed <- subFunCalls e
                          let e_h = hoist e_subbed
                          return $ splitEseq e_h
          -- squash potentially many statements into one (SBlock)
          groupStms [s] = s
          groupStms ss  = SBlock ss

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
                                    return $ ESeq [(SAss (LVal t_i) (EFunCall nm sub_args))]
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


-- return the current Int state, and increment the state
nextInt :: St.State Int Int
nextInt = do i <- St.get
             St.put (i+1)
             return i



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
