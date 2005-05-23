module Generate where

import List

main = unrollFor testforStmt

data Stm = SDecl Var Typ |
           SAss Var Exp |
           SIfElse Exp [Stm] [Stm] |
           SFor Var Exp Exp [Stm]

data Exp = EVar Var |
           EAdd Exp Exp |
         -- literals
           EStr String |
           EDoub Double |
           EInt Int

-- data Literal = LStr String |
--                LDouble Double |
--                LInt Int
--     deriving Show

data Typ = TInt | TDouble
    deriving Show

data Var = VString String
    deriving (Eq)

-- Example syntax tree, for the statement x = y + 12.
--  SAss (VString "x")
--       (EAdd (EVar $ VString "y")
--             (EInt 12))



-- unroll a for-loop
unrollFor :: Stm -> [Stm]
unrollFor for@(SFor _ (EInt lo) (EInt hi) _) = concat $ unfoldr unroll1 (for, lo)
          -- unrolled enough when loop counter exceeds 'hi'
    where unroll1 (_, cur)  | cur > hi               = Nothing
          -- substitute in the value of the loop counter, and then recursively unroll
          unroll1 (for@(SFor var _ _ stmts) , cur)   = let substed  = map (subst var (EInt cur)) stmts
                                                           unrolled = concatMap unrollFor substed
                                                       in Just (unrolled, (for,cur+1))
unrollFor stm                                = [stm] -- non-for statements not unrolled


-- substitute a value for a variable into a statement
subst :: Var -> Exp -> Stm -> Stm
subst var _   (SAss var2 _  )   | var == var2 = error "Assigning to loop variable"
subst var _   (SFor var2 _ _ _) | var == var2 = error "Outer loop var reused as inner loop var"
subst var _   (SDecl var2 _)    | var == var2 = error "Redeclaring loop variable in loop"
subst var val (SFor var2 elo ehi stmts)       = SFor var2
                                                     (substExp var2 val elo)
                                                     (substExp var2 val ehi)
                                                     (map (subst var val) stmts)
subst var val (SAss var2 exp)                 = SAss var2 (substExp var val exp)
subst _   _   stm                             = stm


-- substitue a value for a var into an exp
--          var    val    exp    result
substExp :: Var -> Exp -> Exp -> Exp
substExp var val (EAdd e1 e2)               = (EAdd (substExp var val e1) (substExp var val e2))
substExp var val (EVar var2) | var == var2  = val
substExp _   _   exp                        = exp


showsStm :: Stm -> ShowS
showsStm (SAss var val)         = shows var . ( " = " ++ )  . shows val . ('\n':)
showsStm (SDecl var typ)        = shows var . ( " :: " ++ ) . shows typ . ('\n':)
showsStm (SFor var lo hi stmts) = ("for " ++) .
                                  (shows var) .
                                  (" = "++) .
                                  (shows lo) .
                                  (" to "++) . 
                                  (shows hi) .
                                  (":\n"++)  .
                                  (showList stmts)

instance Show Stm where
    showsPrec _ = showsStm

showsExp :: Exp -> ShowS
showsExp (EVar v)     = shows v
showsExp (EAdd e1 e2) = shows e1 . (" + "++) . shows e2
showsExp (EInt i)     = shows i

instance Show Exp where
    showsPrec _ = showsExp


instance Show Var where
    showsPrec _ (VString s) = (s++)


-- tree for:
-- for (i = 0 to 10) { x = x + i; }
testforStmt = SFor (VString "i")
                   (EInt 0)
                   (EInt 10) 
                   [ SAss (VString "x")
                          (EAdd (EVar $ VString "x")
                                (EVar $ VString "i")),
                     SFor (VString "j")
                          (EInt 0)
                          (EInt 4)
                          [ SAss (VString "x")
                                 (EAdd (EVar $ VString "x")
                                       (EAdd (EVar $ VString "j")
                                             (EVar $ VString "i"))) ]
                   ]
