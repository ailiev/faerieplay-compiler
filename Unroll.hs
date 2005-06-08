module Unroll where


import Control.Monad.Error (Error, noMsg, strMsg)
import Data.Bits
import qualified Control.Monad.State as St
import Control.Monad.Trans (lift)
import Maybe (fromJust)
import qualified Data.Map as Map

import SashoLib (maybeLookup, (<<), ilog2)

import Intermediate
import HoistStms

import qualified TypeChecker as Tc



-- This is the type of our type error representation.
data TypeError = Err {line::Int, reason::String} deriving (Show)

-- We make it an instance of the Error class
instance Error TypeError where
  noMsg    = Err 0 "Type error"
  strMsg s = Err 0 s

type OutMonad = Either TypeError


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


type StateWithErr = St.StateT MyStateT OutMonad


throwErr :: Int -> String -> StateWithErr a
throwErr p msg = lift $ Left $ Err p msg


unrollProg :: Prog -> OutMonad [Stm]
unrollProg (Prog pname pt@(ProgTables {funcs=fs})) =
    let (Func _ t form_args stms) = fromJust $ Map.lookup "main" fs
        startScope = pushScope []
        out = St.runStateT (mapM unroll stms) (pt, startScope)
    in case out of
                Left err        -> Left err
                Right (stmss,_) -> Right (concat stmss)



-- scope invariants:
-- unroll is called with the correct scope depth, but with the top-level scope
-- number of the previous unroll call. Thus, have to increment the scope at the
-- start, and add a new depth before calling unroll recursively
unroll :: Stm -> StateWithErr [Stm]
-- unroll for@(SFor _ (EInt lo) (EInt hi) (SBlock _ _)) = for
unroll s@(SAss lv e@(EFunCall nm args)) = genericUnroll unrollAss s
    where unrollAss scope (SAss lv e@(EFunCall nm args)) =
              do (Func name t form_args stms) <- extractFunc nm
                 -- replace all local variables with Scoped ones, in stms
                 let stms'  = map (scopeVars scope) stms
                 -- substitute actual values for all the formal params, in all
                 -- stms
                 -- substs is a list of subst partial applications:
                 -- substs :: [Stm -> Stm]
                     substs = [subst farg arg | farg <- form_args, arg <- args]
                     stms'' = map (\stm -> foldl (flip ($)) stm substs) stms'
                     -- append an assignment to the target var
                     stms3  = stms'' ++ [SAss lv (fcnRetVar nm scope)]
                 return stms3


-- this is a problem. it should only scope vars which are local to
-- this block. how do we know this? mark it during typecheck?
unroll s@(SBlock stms) = genericUnroll unrollBlock s
    where unrollBlock scope (SBlock stms) =
              do -- replace all local variables with Scoped ones, in stms
                 let stms' = map (scopeVars scope) stms
                 return stms'
{-
unroll s@(SFor id lo hi stms) = genericUnroll unrollFor s
    where unrollFor scope (SFor id lo_exp hi_exp stms) =
              do lo <- evalStatic lo_exp
                 hi <- evalStatic hi_exp
                 let stms' = map (scopeVars scope) stms
                     -- the actual unrolling!
                     stmss = replicate (hi-lo) stms'
                     -- and add correct counter values in there
                     counters = [lo..hi]
                     stmss' = zipWith (subst 
-}
                   

unroll (SFor id lo hi stms) = do stmss <- mapM unroll stms
                                 return [SFor id lo hi (concat stmss)]

unroll s = return [s]



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
                         


checkScopeDepth scope
    | length scope > cMAXSCOPE = throwErr 42 $ "Function recursion deeper than"
                                           << cMAXSCOPE
    | otherwise                = return ()







scopeVars scope s = mapStm f_s f_e s
          -- if already scoped, just overwrite the old scope
    where f_e (EVar v@(VScoped _ _))            = (EVar (doVar v))
          -- only scope non-global vars
          f_e (EVar v)
              | not $ elem Global (vflags v)    = (EVar (doVar v))
          f_e e                                 = e

          f_s (SFor counter lo hi stms)         = SFor (doVar counter) lo hi stms
          f_s s                                 = s

          -- if already scoped, just overwrite the old scope
          doVar (VScoped scope_old v)   = (VScoped scope v)
          doVar v                       = (VScoped scope v)

               

-- to reconstruct the return variable of a function in a given scope
-- FIXME: rather awkward, may be better if a Func carried its return var around
-- explicitly
fcnRetVar fname scope = EVar (VScoped scope (VFlagged [RetVar] (VSimple fname)))


extractFunc name = do ProgTables {funcs=fs} <- St.gets getsPT
                      case maybeLookup name [fs] of
                            (Just f)    -> return f
                            _           -> throwErr 42 $ "Unroll.extractFunc failed!"


{-
-- evalStatic :: Exp -> StateWithErr Integer
evalStatic = mapExpM f
    where f e =
              case e of
                (BinOp op (ELit l1) (ELit l2))  -> evalBinOp op l1 l2
                (UnOp  op (ELit l1))            -> evalUnOp op l1
                (ELit l)                        -> e
                (EGetBit (ELit (LInt x))
                         (ELit (LInt i)))       -> fromIntegral $ testBit x i
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

-}

--unrollStms =  unrollFor

{-
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
-}


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
