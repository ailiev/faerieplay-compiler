-- code to generate a circuit from an unrolled list of statements,
-- consisiting only of SAss, and SIf.

module CircGen where

import Monad (foldM)
import Maybe (fromJust)

import Data.Graph.Inductive.Graph               ((&))
import qualified Data.Graph.Inductive.Graph     as Gr

import qualified Data.Graph.Inductive.Tree      as TreeGr
import qualified Data.Map                       as Map
import qualified Control.Monad.State            as St

import SashoLib (projSnd, projFst, modifyListHead, Stack(..), maybeLookup)

import Intermediate as Im hiding (VarTable)



-- number of inputs, and the list of outputs, one for every input in binary numerical order
-- (CHECK)
-- contains 2^{#inputs} entries, most likely 4 or 8
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
          do testGate           <- getsVars (fromJust . maybeLookup testVar)
             pushScope
             circ'              <- foldM genStm circ stms
             ifScope            <- popScope
             parentScope        <- getVars
             circ''             <- genCondExit testGate circ' parentScope ifScope
             return circ''

-- here we need to be able to tell which variables are local and which
-- are not - the non-local ones need to be propagated to outside this
-- conditional scope, by means of Select gates keyed on the conditional
-- 
-- solution! the non-local vars will be found in lower layers of the
-- scope stack. if they're not, they must be local!
genCondExit testGate circ parentScopeVars thisScopeVars =
    let vars = Map.toList thisScopeVars
    in  do foldM handleVar circ vars
    where handleVar c (var,gate) = case maybeLookup var parentScopeVars of
                                     Nothing            -> return c
                                     (Just parentGate)  -> addSelect c var gate parentGate
                                                              
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


genExp circ exp = undefined
             
-- make an unlabelled edge
uAdj n = ((), n)


-- make a graph Context for this operator, node number and source gates
mkCtx node op srcs = let g = Gate node op srcs
                         ctx = (map uAdj srcs,
                                node,
                                g,
                                [])
                     in ctx

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
