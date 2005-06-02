module Unroll where


import Intermediate

import TypeChecker as Tc

unrollStms = concatMap unrollFor


-- unroll a for-loop
unrollFor :: Stm -> [Stm]
unrollFor for@(SFor _ (EInt lo) (EInt hi) (SBlock _ _)) = concat $ unfoldr unroll1 (for, lo)
          -- unrolled enough when loop counter exceeds 'hi'
    where unroll1 (_, cur)  | cur > hi               = Nothing
          -- substitute in the value of the loop counter, and then recursively unroll
          unroll1 (for@(SFor var _ _ stm) , cur)   =
              let substed  = (subst var (EInt cur)) stmts
                  unrolled = concatMap unrollFor substed
              in Just (unrolled, (for,cur+1))
          unroll1 (for@(SFor _ _ _ stm) , cur)                  =
              Just    ([stm], (for,cur+1))
unrollFor stm                                = [stm]

-- substitute a value for a variable into a statement

subst :: Ident -> Exp -> Stm -> Stm
subst var val s =
    let subs = subst var val
        sube = substExp var val
    in case s of
              (SFor var2 _ _ _) | var == var2   -> error "Outer loop var reused as inner loop var"
              (SFor var2 elo ehi stm)           -> SFor var2 (sube elo) (sube ehi) (subs stm)
              (SIf test stm)                    -> SIf (sube test) (subs stm)
              (SIfElse test stm1 stm2)          -> SIfElse (sube test) (subs stm1) (subs stm2)
              -- note: typechecking should have caught assignment to
              -- immutable vars, like loop counters
              (SAss var2 exp)                   -> SAss var2 (sube exp)
              (SBlock stms)                     -> SBlock (map subs stms)



-- substitue a value for a var into an exp
--          var    val    exp    result
substExp :: Ident -> Exp -> Exp -> Exp
substExp var val e =
    let sub = substExp var val in
    case e of
           (EVar var2) | var == var2    -> val
           (EStruct e field)            -> EStruct (sub e) field
           (BinOp op e1 e2)             -> BinOp op (sub e1) (sub e2)
           (UnOp  op e)                 -> UnOp  op (sub e)
           (EArr arr idx)               -> EArr (sub arr) (sub idx)
           (EFunCall nm args)           -> EFunCall nm (map sub args)
           (EGetBit v bit)              -> EGetBit (sub v) (sub bit)
           (EBitsize e)                 -> EGetBit (sub v) (sub bit)
           (EStatic e)                  -> EStatic (sub e)
           (ExpT t e)                   -> ExpT t (sub e)
           -- the rest are literals etc, no need to recurse
           e                            -> e

