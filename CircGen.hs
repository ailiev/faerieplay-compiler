-- code to generate a circuit from an unrolled list of statements,
-- consisiting only of SAss, and SIf.

module CircGen where

import Monad (foldM)
import Maybe (fromJust, isJust)
import List

import Debug.Trace      (trace)

import Data.Graph.Inductive.Graph               ((&))
import qualified Data.Graph.Inductive.Graph     as Gr
import qualified Data.Graph.Inductive.Graphviz  as Graphviz
import qualified Data.Graph.Inductive.Tree      as TreeGr
import qualified Data.Map                       as Map
import qualified Control.Monad.State            as St

import SashoLib (projSnd, projFst, modifyListHead, Stack(..),
                 maybeLookup, (<<), mapTuple2)
import qualified Container as Cont

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
                 GateDoc        -- some documentation, eg. a var name

type GateDoc = Maybe Var


{-
-- we will have Forward edges denoting the flow of data through the
-- circuit, and Back edges so a vertex (Gate) can see where its inputs are
data EdgeKind = Forward | Back
-}

-- NOTE: the gate number is the same as the Node value in the Graph
-- (which is also an Int)
-- helper to make a labelled Node from a Gate
gate2node :: Gate -> Gr.LNode Gate
gate2node g@(Gate i _ _ _) = (i,g)


-- needed to lookup a variable's current gate number
type VarTable = Map.Map Var Int

type MyState = ([VarTable],
                Int)

-- type OutMonad a = St.State MyState a
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



-- create the input gates for this argument to main(), and add the vars
-- to the top-level VarTable
-- will make this return a [Gate], for when we deal with arrays etc
createInputGates (TypedName typ@(IntT _) name)
    = do i <- nextInt
         -- this is a formal parameter, of main()
         let var = add_vflags [FormalParam] (VSimple name)
         modifyVars $ modtop $ Map.insert var i
         return [Gate i (Input typ) [] (Just var)]


genStm :: Circuit -> Stm -> OutMonad Circuit
genStm circ stm =
    case stm of
      -- TODO: the lval expression may not be an EVar, could be EArr etc.
      (SAss (EVar var) exp)     -> do (circ', gateNum) <- genExp circ exp
                                      let circ'' = addGateDoc gateNum circ' var
                                      modifyVars $ modtop $ Map.insert var gateNum
                                      return circ''
      -- NOTE: after HoistStm.hs, all conditional tests are EVar
      (SIfElse (EVar testVar)
               (locs1, stms1)
               (locs2, stms2))  ->
          do testGate           <- getsVars $ fromJustMsg "finding conditional test gate" .
                                              maybeLookup testVar
             -- do the recursive generation for both branches, and
             -- save the resulting var scopes
             pushScope
             circ1'             <- foldM genStm circ stms1
             ifScope            <- popScope
             pushScope
             circ2'             <- foldM genStm circ1' stms2
             elseScope          <- popScope

             parentScope        <- getVars
             -- generate the conditional exit gates
             circ''             <- genCondExit testGate
                                               circ2'
                                               (parentScope, ifScope, elseScope)
                                               (locs1, locs2)
             return circ''
    where addGateDoc gate circ var
              = updateLabel (\(Gate i op srcs _) -> Gate i op srcs (Just var))
                            circ
                            gate


-- update the label for a given node in a graph. will call error if
-- this node is not in the graph
updateLabel :: (Gr.DynGraph gr) => (a -> a) -> gr a b -> Gr.Node -> gr a b
updateLabel f gr node = let (mctx,gr')                  = Gr.match node gr
                            (e_in, node', lab, e_out)   = fromJust mctx
                        in  (e_in, node', f lab, e_out) & gr'
                            

-- generate the gates needed when exiting a conditional block---to
-- conditionally update free variables updated in this scope
--
-- NOTE: the vars in the locals VarSets (ifLocs and elseLocs)
-- are without scopes
genCondExit testGate
            circ
            (parentScope, ifScope, elseScope)
            (ifLocs, elseLocs) =
    let vars    = List.nub $
                  filter nonLocal $
                  map (\(var,gate) -> var) $
                  concatMap Map.toList [ifScope, elseScope]
        sources = map varSources $ trace ("non-local scope vars: " << vars) vars
    in  foldM addSelect circ $ zip vars sources
          
    where -- a var is non-local if was not declared in this scope,
          -- *and* it appears in the parent scope (needed in the case of
          -- generated vars)
          nonLocal var = (not $ any (Cont.member (stripScope var))
                                    [ifLocs, elseLocs])            &&
                         (isJust $ maybeLookup var parentScope)
                          
          -- return the gate number where this var can be found, if the cond
          -- is true, and if it's false. This depends on which branch
          -- (or both) it was updated in
          varSources var = mapTuple2 (gate var)
                                    (case map (Map.member var) [ifScope, elseScope] of
                                       [True,  True]       -> ([ifScope],   [elseScope])
                                       [True,  False]      -> ([ifScope],   parentScope)
                                       [False, True]       -> (parentScope, [elseScope]))
          gate var scopes = fromJust $ maybeLookup var scopes

          -- add a Select gate for a free variable, and update its
          -- wire location, to the new Select gate
          addSelect c (var, (gate_true, gate_false))
              = do i <- nextInt
                   let srcs = [testGate,
                               gate_true,
                               gate_false]
                       ctx = mkCtx i Select srcs (Just var)
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
                              let ctx = mkCtx i (Bin op) [gate1, gate2] Nothing
                              return (ctx & c2', i)
      (UnOp op e1)      -> do i <- nextInt
                              (c1', gate1) <- genExp c   e1
                              let ctx = mkCtx i (Un op) [gate1] Nothing
                              return (ctx & c1', i)
      (EVar var)        -> do mb_gate <- getsVars $ maybeLookup var
                              case mb_gate of
                                (Just gate)     -> return (c, gate)
    -- an uninitialized reference! FIXME: for now just init to literal
    -- zero (ie. 42) and return the new gate number
                                Nothing         -> do i <- nextInt
                                                      let ctx = mkCtx i (Lit (LInt 42)) [] (Just var)
                                                      modifyVars $ modtop $ Map.insert var i
                                                      return (ctx & c, i)
      (ELit l)          -> do i <- nextInt
                              let ctx = mkCtx i (Lit l) [] Nothing
                              return (ctx & c, i)
      (ExpT _ e)        -> genExp c e
      (EStatic e)       -> genExp c e
-- TODO: EArr and EStruct
                              
             
-- make an unlabelled edge
uAdj n = ((), n)


-- make a graph Context for this operator, node number and source gates
mkCtx node op srcs doc = let g   = Gate node op srcs doc
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
-- modifyInt f = St.modify (projSnd f)
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
    show (Gate i op srcs doc) = i << ": " << op << (case doc of
                                                             Just var   -> " (" << var << ")"
                                                             Nothing    -> "")

instance Show Op where
    show (Bin op) = show op
    show (Un  op) = show op
    show (Input t) = "in " << t
    show Select   = "sel"
    show (Lit l)        = "Lit " << l


showCct c = Graphviz.graphviz' c
 