module Unroll where

import qualified Control.Monad.State as St

import Data.Bits

import Intermediate

import TypeChecker as Tc



-- This is the type of our type error representation.
data TypeError = Err {line::Int, reason::String} deriving (Show)

-- We make it an instance of the Error class
instance Error TypeError where
  noMsg    = Err 0 "Type error"
  strMsg s = Err 0 s

type OutMonad = Either TypeError


type MyStateT = ProgTables

type StateWithErr = St.StateT MyStateT OutMonad



unroll :: Stm -> StateWithErr [Stm]
unroll for@(SFor _ (EInt lo) (EInt hi) (SBlock _ _)) = for
unroll (SAss (LVal (EVar v)) e@(EFunCall nm args)) =
    do (Func t vars stms) <- Tc.extractFunc e nm
       new_stms <- mapM subst 
       


evalStatic :: Exp -> StateWithErr Integer
evalStatic = mapExpM f
    where f e =
              case e of
                (BinOp op (ELit l1) (ELit l2))  -> evalBinOp op l1 l2
                (UnOp  op (ELit l1))            -> evalUnOp op l1
                (ELit l)                        -> evalLit l
                (EGetBit (ELit (LInt x)
                         (ELit (LInt i))        -> fromIntegral $ testBit x i
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
                         

evalLit (LInt i)  = i
evalLit (LBool b) = fromIntegral b



--unrollStms =  unrollFor


-- unroll a for-loop
unrollFor :: Stm -> [Stm]
unrollFor for@(SFor _ lo hi s) = let 

concat $ unfoldr unroll1 (for, lo)
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
