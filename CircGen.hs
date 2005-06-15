-- code to generate a circuit from an unrolled list of statements,
-- consisiting only of SAss, and SIf.

module CircGen where

import Monad (foldM)
import Maybe (fromJust)

import Debug.Trace      (trace)

import Data.Graph.Inductive.Graph               ((&))
import qualified Data.Graph.Inductive.Graph     as Gr
import qualified Data.Graph.Inductive.Graphviz  as Graphviz
import qualified Data.Graph.Inductive.Tree      as TreeGr
import qualified Data.Map                       as Map
import qualified Control.Monad.State            as St

import SashoLib (projSnd, projFst, modifyListHead, Stack(..), maybeLookup, (<<))

import Intermediate as Im hiding (VarTable)



data Op =
    Bin Im.BinOp                -- binary operator
  | Un  Im.UnOp                 -- unary operator
  | ReadDynArray                -- read from a dynamic array (2
                                -- inputs: array and index)
  | WriteDynArray               -- update array; 3 inputs
  | Input Typ                   -- an input gate, and its simple type,
                                -- ie. IntT or BoolT
  | Select                      -- select one of two values based on a
                                -- test; the input wires are to be:
                                -- [test, src_true, src_false]
  | Lit Im.Lit                  -- a literal


--          

-- QUESTION: do we include as part of Gate the numbers of the input
-- Gate's? That info should be available in the graph structure, BUT
-- the order of in-edges is important for non-commutative operators,
-- whereas in standard graphs the order of edges is immaterial. For
-- now I do include the input gate numbers
data Gate = Gate Int            -- the gate number
                 Op             -- gate operation (+,*, & etc)
                 [Int]          -- numbers of the input gates, in the
                                -- right order. may do away with this
                                -- if the graph can mainatain
                                -- edge-order

{-
-- we will have Forward edges denoting the flow of data through the
-- circuit, and Back edges so a vertex (Gate) can see where its inputs are
data EdgeKind = Forward | Back
-}

-- NOTE: the gate number is the same as the Node value in the Graph
-- (which is also an Int)
-- helper to make a labelled Node from a Gate
gate2node :: Gate -> Gr.LNode Gate
gate2node g@(Gate i _ _) = (i,g)


-- needed to lookup a variable's current gate number
type VarTable = Map.Map Var Int

type MyState = ([VarTable],
                Int)

type OutMonad = St.State MyState


type Circuit = TreeGr.Gr Gate ()


genCircuit :: [Stm] ->          -- the Unroll'd [Stm]
              [TypedName] ->    -- parameters of main()
              Circuit           -- the resulting circuit

genCircuit stms args = let startState = ([Map.empty], 0)
                           (out, st) = St.runState (genCircuitM stms args)
                                                   startState
                       in out



genCircuitM :: [Stm] -> [TypedName] -> OutMonad Circuit
genCircuitM stms args = do inputs <- genInputs args
                           foldM genStm inputs stms


genInputs :: [TypedName] -> OutMonad Circuit
genInputs names = do gatess <- mapM createInputGates names
                     return (Gr.mkGraph (map gate2node (concat gatess)) [])

{-
genInputs names = do let gates' = concatMap createInputGates names
                         gr     = Gr.empty
                         nodes  = Gr.newNodes (length gates) gr
                         gates  = zipWith completeGate gates' nodes
                     mapM addVar gates
                     return $ Gr.mkGraph gates []
    where completeGate (Gate _ op inps) i = (i, Gate i op inps)
-}


-- create the input gates for this argument to main(), and add the vars
-- to the top-level VarTable
-- will make this return a [Gate], for when we deal with arrays etc
createInputGates (TypedName typ@(IntT _) name)
    = do i <- nextInt
         modifyVars $ modtop $ Map.insert (VSimple name) i
         return [Gate i (Input typ) []]


genStm :: Circuit -> Stm -> OutMonad Circuit
genStm circ stm =
    case stm of
      -- TODO: the lval expression may not be an EVar, could be EArr etc.
      (SAss (EVar var) exp)     -> do (circ', gateNum) <- genExp circ exp
                                      modifyVars $ modtop $ Map.insert var gateNum
                                      return circ'
      -- NOTE: after HoistStm.hs, all conditional tests are EVar
      (SIf (EVar testVar) stms)     ->
          do testGate           <- getsVars $ fromJustMsg "finding conditional test gate" . maybeLookup testVar
             pushScope
             circ'              <- foldM genStm circ stms
             thisScope          <- popScope
             parentScope        <- getVars
             circ''             <- genCondExit testGate circ' parentScope thisScope
             return circ''


-- generate the gates needed when exiting a conditional block---to
-- conditionally update free variables updated in this scope
-- 
-- here we need to be able to tell which variables are local and which
-- are not - the non-local ones need to be propagated to outside this
-- conditional scope, by means of Select gates keyed on the conditional
-- 
-- solution! the non-local vars will be found in lower layers of the
-- scope stack (parentScopeVars). if they're not, they must be local!
genCondExit testGate circ parentScopeVars thisScopeVars =
    let vars = Map.toList thisScopeVars
    in  foldM handleVar circ (trace ("scope vars " << vars) vars)
          
    where -- deal with one locally updated variable (free or not)
          -- FIXME: shadowing of vars is not currently handled!
          -- the only way to tell that a var updated here is shadowing
          -- another var in a outside scope (and thus its value should
-- not be propagated outside this scope) is by the var declaration,
          -- so looks like that info (ie. local var info) has to be propagated all the way
          -- from the TypeChecker
          handleVar c (var,gate) = case maybeLookup var (trace ("parent vars: " << parentScopeVars) parentScopeVars) of
                                     Nothing            -> return c
                                     (Just parentGate)  -> addSelect c var gate parentGate
          -- add a Select gate for a free variable, and update its
          -- wire location, to the new Select gate
          addSelect c var gate parentGate
              = do i <- nextInt
                   let srcs = [testGate,
                               gate,       -- if test == True
                               parentGate] -- False
                       ctx = mkCtx i Select srcs
    -- update the location of 'var'. this is a bit of a hack - it calls
    -- Map.adjust on all the VarTable's in the stack, but will end up
    -- updating just the one where the Var actually is
                   modifyVars $ map $ Map.adjust (const i) var
                   return (ctx & c)


-- adds the needed gates to the circuit, and returns at which gate/node
-- number the result is
genExp :: Circuit -> Exp -> OutMonad (Circuit, Gr.Node)
genExp c exp =
    case exp of
      (BinOp op e1 e2)  -> do i <- nextInt
                              (c1', gate1) <- genExp c   e1
                              (c2', gate2) <- genExp c1' e2
                              let ctx = mkCtx i (Bin op) [gate1, gate2]
                              return (ctx & c2', i)
      (UnOp op e1)      -> do i <- nextInt
                              (c1', gate1) <- genExp c   e1
                              let ctx = mkCtx i (Un op) [gate1]
                              return (ctx & c1', i)
      (EVar var)        -> do mb_gate <- getsVars $ maybeLookup (strip_vflags var)
                              case mb_gate of
                                (Just gate)     -> return (c, gate)
    -- an uninitialized reference! FIXME: for now just init to literal
    -- zero (ie. 42) and return the new gate number
                                Nothing         -> do i <- nextInt
                                                      let ctx = mkCtx i (Lit (LInt 42)) []
                                                      modifyVars $ modtop $ Map.insert var i
                                                      return (ctx & c, i)
      (ELit l)          -> do i <- nextInt
                              let ctx = mkCtx i (Lit l) []
                              return (ctx & c, i)
      (ExpT _ e)        -> genExp c e
      (EStatic e)       -> genExp c e
-- TODO: EArr and EStruct
                              
             
-- make an unlabelled edge
uAdj n = ((), n)


-- make a graph Context for this operator, node number and source gates
mkCtx node op srcs = let g = Gate node op srcs
                         ctx = (map uAdj srcs,
                                node,
                                g,
                                []) -- no outgoing edges
                     in ctx


-- extract the params of main() from a Prog
-- FIXME: Func does not now hang on to the types of the function
-- parameters, which we do need here.
-- for now invent a type :)
extractInputs (Prog pname (ProgTables {funcs=fs})) =
    let (Func _ _ t form_args stms) = fromJust $ Map.lookup "main" fs
    in  map (var2TN . strip_vflags) form_args
    where var2TN (VSimple name) = TypedName (IntT 32) name


--------------------
-- state access functions
--------------------
getsVars f = St.gets $ f . fst

getInt :: OutMonad Int
getInt = St.gets snd

getVars :: OutMonad [VarTable]
getVars = getsVars id

-- modify the various parts of the state with some function
modifyInt  = St.modify . projSnd
modifyVars = St.modify . projFst


--------------------
-- state utility functions
--------------------
nextInt :: OutMonad Int
nextInt = do modifyInt (+1)
             getInt

pushScope = modifyVars $ push Map.empty
-- pop the scope stack, and return the top scope/VarTable
popScope  = do scope <- getsVars peek
               modifyVars pop
               return scope


fromJustMsg msg (Just x) = x
fromJustMsg msg Nothing  = error $ "fromJust Nothing! " << msg



instance Show Gate where
    show (Gate i op srcs) = i << ": " << op

instance Show Op where
    show (Bin op) = show op
    show (Un  op) = show op
    show (Input t) = "in " << t
    show Select   = "sel"
    show (Lit l)        = "Lit " << l


showCct c = Graphviz.graphviz' c
 