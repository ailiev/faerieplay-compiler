module Unroll where


import Intermediate as Im

import TypeChecker as Tc

unrollStms = concatMap unrollFor


-- unroll a for-loop
unrollFor :: Im.Stm -> [Im.Stm]
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
unrollFor for@(SFor _ _ _ stm)               = [stm]
unrollFor stm                                = [stm] -- non-for statements not unrolled

-- substitute a value for a variable into a statement

subst :: Ident -> Exp -> Stm -> Stm
-- subst var _   (SAss var2 _  )   | var == var2 = error "Assigning to loop variable"
subst var _   (SFor var2 _ _ _) | var == var2 = error "Outer loop var reused as inner loop var"
--subst var _   (VarDecl _ var2)    | var == var2 = error "Redeclaring loop variable in loop"
subst var val (SFor var2 elo ehi stm)       = SFor var2
                                                   (substExp var val elo)
                                                   (substExp var val ehi)
                                                   (subst var val stm)
subst var val (SIf test stm)                = SIf (substExp var val test)
                                                    (subst var val stm)
subst var val (SIfElse test stm1 stm2)      = SIfElse (substExp var val test)
                                                      (subst var val stm1)
                                                      (subst var val stm2)
subst var val (SAss var2 exp)                 = SAss var2 (substExp var val exp)
subst var val (SBlock d stms)                 = SBlock d (map (subst var val) stms)
-- subst _   _   stm                             = stm


-- substitue a value for a var into an exp
--          var    val    exp    result
substExp :: Ident -> Exp -> Exp -> Exp
substExp var val (EVar var2) | var == var2  = val
-- one entry for binary operations, with the help of extrBinOp
-- this whole rigmarole is because Haskell will not take:
-- substExp var val (eBinOp e1 e2)              = (eBinOp  (substExp var val e1) (substExp var val e2))
-- ie. a variable for the data constructor
-- substExp var val exp | isJust fields = let (op, e1, e2) = fromJust fields
--                                        in  op (substExp var val e1) (substExp var val e2)
--                      where fields = extrBinOpFields exp
-- and using GHC pattern guards:
substExp var val exp
    | Just (op, e1, e2) <- extrBinOpFields exp = op (substExp var val e1) (substExp var val e2)
substExp var val (EArr e idx)               = EArr (substExp var val e) (substExp var val idx)
substExp var val (EStruct e efield)         = EStruct (substExp var val e) (substExp var val efield)

substExp _   _   exp                        = exp

