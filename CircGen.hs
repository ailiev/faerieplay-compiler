-- code to generate a circuit from an unrolled list of statements,
-- consisiting only of SAss, and SIfElse.

module CircGen (
                genCircuit,
                extractInputs,
                showCct
               ) where


import Monad (foldM)
import Maybe (fromJust, isJust, fromMaybe)
import List

import Debug.Trace      (trace)

import Data.Graph.Inductive.Graph               ((&))
import qualified Data.Graph.Inductive.Graph     as Gr
import qualified Data.Graph.Inductive.Graphviz  as Graphviz
import qualified Data.Graph.Inductive.Tree      as TreeGr
import qualified Data.Graph.Inductive.Query.DFS as GrDFS
import qualified Data.Map                       as Map
import qualified Control.Monad.State            as St

import SashoLib

import qualified Container as Cont

import Intermediate as Im hiding (VarTable)


data GateFlags = Output

data Op =
    Bin Im.BinOp                -- binary operator
  | Un  Im.UnOp                 -- unary operator
  | ReadDynArray                -- read from a dynamic array
                                -- inputs: [array, index];
                                -- output is of the array element type
  | WriteDynArray               -- update array; inputs = [array, index, value]
                                -- output is the updated array
  | Input                       -- an input gate
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
                 Typ            -- the output type of the gate
                 Op             -- gate operation (+,*, & etc)
                 [Int]          -- numbers of the input gates, in the
                                -- right order. may do away with this
                                -- if the graph can mainatain
                                -- edge-order
                 [GateFlags]
                 GateDoc        -- some documentation, eg. a var name

type GateDoc = Maybe [Exp]


setFlags newfl (Gate g1 g2 g3 g4 fl    g6) =
               (Gate g1 g2 g3 g4 newfl g6)

setGateType newt        (Gate g1 typ  g3 g4 g5 g6) =
                        (Gate g1 newt g3 g4 g5 g6)


-- NOTE: the gate number is the same as the Node value in the Graph
-- (which is also an Int)
-- helper to make a labelled Node from a Gate
gate2lnode :: Gate -> Gr.LNode Gate
gate2lnode g@(Gate i _ _ _ _ _) = (i,g)

-- just the node number of a Gate
gateNode = fst . gate2lnode



-- needed to lookup a variable's current gate number
-- Structs and arrays occupy multiple gates, so need to actually store
-- a list of gate numbers
type VarTable = Map.Map Var [Int]


-- the state in these computations:
type MyState = ([VarTable],     -- stack of table Var->Node
                Int)            -- a counter to number the gates
                                -- sequentially


-- type OutMonad a = St.State MyState a
type OutMonad = St.State MyState


-- nodes are labelled with a Gate, and the edges are for now unlabelled
type Circuit = TreeGr.Gr Gate ()
type CircuitCtx = Gr.Context Gate ()



-- the main function here
{-
genCircuit :: TypeTable ->      -- the type table
              [Stm] ->          -- the Unroll'd [Stm]
              [TypedName] ->    -- parameters of main()
              Circuit           -- the resulting circuit
-}

genCircuit type_table stms args =
    let startState = ([Map.empty], 0)
        (circ, st) = St.runState (genCircuitM type_table stms args)
                                 startState
        top_circ   = GrDFS.topsort' circ
    in circ





-- and the stateful computation
genCircuitM :: TypeTable -> [Stm] -> [TypedName] -> OutMonad Circuit
genCircuitM type_table stms args =
    do input_gates <- genInputs type_table args
       foldM genStm input_gates stms


genInputs :: TypeTable -> [TypedName] -> OutMonad Circuit
genInputs type_table names = do gatess <- mapM (createInputGates type_table True) names
                                return (Gr.mkGraph (map gate2lnode (concat gatess)) [])


-- fully expand a type, recursing into complex types
expandType type_table t =
    case t of
      (SimpleT name)    -> let t' = fromJust $ Map.lookup name type_table
                           in  expandType type_table t'
      (ArrayT elem_t
              len)      -> ArrayT (expandType type_table elem_t) len
      (StructT fields)  -> StructT (map (projSnd (expandType type_table)) fields)
      _                 -> t


-- create the input gates for this argument to main(), and add the vars
-- to the top-level VarTable
-- will make this return a [Gate], for when we deal with arrays etc
--
-- we'll only insert mappings into the VarTable at the top level, not
-- in recursive calls (then we'd be inserting struct field names as
-- actual vars)

createInputGates :: TypeTable -> Bool -> TypedName -> OutMonad [Gate]
createInputGates type_table addvars (name, typ) =
    let recurse         = createInputGates type_table
        var             = add_vflags [FormalParam] (VSimple name)
    in  case typ of
      (StructT fields)
                -> do gates <- concatMapM (recurse False) fields
                      let is = map gateNode gates
                      if addvars then insertVar var is else return ()
                      return gates

      (SimpleT tname)
                -> let typ' = fromJust $ Map.lookup tname type_table
                   in  recurse True (name, typ')

      -- IntT, BoolT, ArrayT (for dynamic arrays)
      _         -> do i <- nextInt
                      if addvars
                        then trace ("inserting " << var) $ insertVar var [i]
                        else return ()
                      return [mkGate type_table i typ [EVar var]]

    where mkGate type_table i typ doc_exp = Gate i
                                                 (expandType type_table typ)
                                                 Input
                                                 []
                                                 []
                                                 (Just $ doc_exp)





-- this just adds a dummy entry of the correct size in the var table for this
-- lval
-- we only need to do it for structs/arrays which are variables, and not
-- expressions (ie. parts of other complex types)
genComplexInit lval size =
    case lval of
      (EVar var)        -> insertVar var (replicate size (-1))
      _                 -> return ()


-- update a range (offset and length) of a list
listUpdate (offset,len) news l = (take offset l) ++
                                 news ++
                                 (drop (offset + len) l)


-- get the gates corresponding to expression 'e', if necessary adding
-- new gates to the circuit, and add e_doc as the annotation of that
-- gate, if it is newly generated
-- returns the new circuit, and the gates where 'e' is
-- also have the gates pass through a 'ghook' which may update them, eg. add
-- flags
genExpWithDoc c ghook e e_doc =
    do (c', res) <- genExp' c e
       let (c'', nodes) = case res of
                            (Left ctx) ->
                            -- we got a Context back, so
                            -- add the doc to the gate, and apply the
                            -- hook
                                let gate = Gr.lab' ctx
                                    gate' = processGate gate
                                    ctx' = updateCtxLab ctx gate'
                                in  (ctx' & c', [Gr.node' ctx'])

                            (Right nodes) ->
                                -- no new gate generated, but we will
                                -- update the doc and apply the hook
                                -- FIXME: if ghook is a noop this will
                                -- do a lot of graph reconstruction for nothing
                                let c'' = foldl (updateLabel processGate)
                                                c'
                                                nodes
                                              
                                in  (c'', nodes)
       return (c'', nodes)

    where addGateDoc exp (Gate g1 g2 g3 g4 g5 doc) =
                          Gate g1 g2 g3 g4 g5 new_doc
              where new_doc = applyToMaybe (push exp) [exp] doc
          processGate g = let g'  = g `fromMaybe` (ghook g)
                              g'' = addGateDoc e_doc g'
                          in  g''


updateCtxLab (ins,node,label,    outs)  new_label = 
             (ins,node,new_label,outs)


-- get the offset of an expression, relative to the simple variable
-- under which this expression is, eg. for a struct expression x.y.z we want the
-- offset of field 'y.z' under struct 'x'; also return what is the root
-- variable, 'x' in this example
getRootvarOffset (EVar v)                  = (v, 0)
getRootvarOffset (EStruct str_e (off,len)) = let (r_v, off') = getRootvarOffset str_e
                                             in  (r_v, off' + off)
getRootvarOffset (ExpT t e)                = getRootvarOffset e
getRootvarOffset (EArr _ _) = error "getRootvarOffset (EArr ) unimplemented"



genStm :: Circuit -> Stm -> OutMonad Circuit
genStm circ stm =
    case stm of
      -- do away with lval type annotations for now
      (SAss (ExpT _ lval) val) -> genStm circ (SAss lval val)
             
      -- this is a message from the typechecker that a Struct or such
      -- will be coming along just now.
      (SAss lval (ExpT _ (EComplexInit size))) ->
          do genComplexInit lval size
             return circ

      (SAss lval@(EVar var) exp) ->
          do circ' <- checkOutputVars circ var Nothing
             (c', nodes) <- genExpWithDoc circ'
                                          (addOutputFlag var)
                                          exp
                                          lval
             insertVar var nodes
             return c'

      (SAss lval@(EStruct str_e (off,len)) val) ->
             -- get the gates for this 'val', and mark them as holding
             -- this lval from now
          do let (rv, off_e) = getRootvarOffset str_e
                 this_off    = off_e + off
             c2 <- checkOutputVars circ rv (Just (this_off,len))
             (c', gates) <- genExpWithDoc c2
                                          (addOutputFlag rv)
                                          val
                                          lval
             updateVar (listUpdate (this_off,len) gates) rv
             return c'



      -- TODO: the lval expression could be EArr.

      -- NOTE: after HoistStm.hs, all conditional tests are EVar
      (SIfElse test@(EVar testVar)
               (locs1, stms1)
               (locs2, stms2))  ->
          do [testGate]         <- getsVars $ fromJustMsg "finding conditional test gate" .
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

      s -> error $ "Unknow genStm on " << s
    where addOutputFlag var gate =
              case strip_var var of
                (VSimple "main") -> trace ("adding outflag in stm " << stm) $
                                    Just $ setFlags [Output] gate
                _                -> Nothing


-- see if this var is an output var, and if so remove the Output flag
-- on its current gates
-- optionally an offset and length to limit the flag removal to some
-- of the gates, in case only part of a complex output var is being
-- modified.
checkOutputVars :: Circuit -> Var -> Maybe (Int,Int) -> OutMonad Circuit
checkOutputVars c var mb_gate_loc
    | strip_var var == VSimple "main" =
        -- remove the output flags there
        do vgates <- getsVars $
                     fromJustMsg "CircGen: genStm: lookup main" .
                     maybeLookup var
           -- take a slice of the gates if mb_gate_loc is not Nothing
           let vgates' = vgates `fromMaybe`
                         (do (off,len) <- mb_gate_loc
                             return $ take len $ drop off vgates)
               c' = foldl (updateLabel (setFlags []))
                          c
                          vgates'
           return c'
    | otherwise =
        return c


-- update the label for a given node in a graph, if this label is present
updateLabel :: (Gr.DynGraph gr) => (a -> a) -> gr a b -> Gr.Node -> gr a b
updateLabel f gr node = let (mctx,gr')                  = Gr.match node gr
                        in  case mctx of
                              Nothing                           -> gr
                              Just (e_in, node', lab  , e_out)  ->
                                   (e_in, node', f lab, e_out) & gr'


                            

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
                  map (\(var,gates) -> var) $
                  concatMap Map.toList [ifScope, elseScope]
        sources = map varSources $ trace ("non-local scope vars: " << vars) vars
    in  foldM addSelect circ $ zip vars sources
          
    where -- a var is non-local if was not declared in this scope,
          -- *and* it appears in the parent scope (needed in the case of
          -- generated vars)
          nonLocal var = (not $ any (Cont.member (stripScope var))
                                    [ifLocs, elseLocs])            &&
                         (isJust $ maybeLookup var parentScope)
                          
          -- return a pair with the the gate numbers where this var
          -- can be found, if the cond
          -- is true, and if it's false. This depends on which branch
          -- (or both) it was updated in
          varSources var = let scopes@(scopes_true, scopes_false) =
                                   (case map (Map.member var) [ifScope, elseScope] of
                                       [True,  True]       -> ([ifScope],   [elseScope])
                                       [True,  False]      -> ([ifScope],   parentScope)
                                       [False, True]       -> (parentScope, [elseScope]))
                               out   @(gates_true, gates_false) = mapTuple2 (gates var)
                                                                            scopes
                                    
                           in  out

          -- find the gates for a var, looking in the given scope stack
          gates var scopes = fromJust $ maybeLookup var scopes

          -- add Select gates for a free variable, and update its
          -- wire locations, to the new Select gates
          -- need multiple select gates if it's a struct, but in this
          -- case we add Select only for the gates which were actually
          -- updated in this scope
          -- this function is quite nasty!
          addSelect c (var, in_gates@(gates_true', gates_false'))
              = do let changed_gates = filter (\(x,y) -> x /= y) $ uncurry zip in_gates
                       ts        = map (getSelType c) changed_gates
          -- get the right number of new int's
                   is           <- replicateM (length ts) nextInt
                   let ctxs      = zipWith3 mkCtx' is ts changed_gates
                       new_gates = foo (uncurry zip in_gates) is
                   updateVar (const new_gates) var
                   -- work all the new Contexts into circuit c
                   return (foldl (flip (&)) c ctxs)

          mkCtx' i t (true_gate,false_gate) =
              let src_gates = [testGate, true_gate, false_gate]
              in  mkCtx (Gate i t Select src_gates [] Nothing)


-- take a list of pairs, and where a pair is equal, pass on that value, but
-- where they're not equal, use the next value from the second list.
-- ideally, the number  of non-equal pairs should be the same as the lenght of
-- the replacement list
foo :: (Eq a) => [(a,a)] ->    [a] -> [a]
foo    ((x,y):rest)  (z:zs)
    | x == y            = (x : foo rest (z:zs))
    | otherwise         = (z : foo rest zs)
-- in case we run out of replacements prematurely, just use the first of the
-- pair for the rest (arbitrarily)
foo    ((x,y):rest) []  = (x : map fst rest)
foo [] _  = []




-- figure out the type of a Select gate, based on the type of its two
-- inputs
getSelType gr (node1, node2)
    = case map (getType . fromJust . Gr.lab gr) [node1, node2] of
        [Im.BoolT,   Im.BoolT  ]  -> Im.BoolT
        [Im.IntT i1, Im.IntT i2]  -> Im.IntT $ Im.BinOp Max i1 i2

    where getType (Gate _ t _ _ _ _) = t



-- adds the needed gates to the circuit (if any), and returns at which
-- gate/node numbers the result is
genExp :: Circuit -> Exp -> OutMonad (Circuit, [Gr.Node])
genExp c e = do (c', res) <- genExp' c e
                case res of
                  -- extend the graph with the new context, and return it
                  (Left ctx@(_,node,_,_))       -> return (ctx & c' , [node])
                  -- just pass on the graph and node
                  (Right gateNums)              -> return (c'       , gateNums)
                  

-- this one is a little nasty - it returns the expanded circuit, and
-- either the context for this expression, which can be processed and
-- added to the circuit by the caller, or the gate number where this
-- expression can be found (in case it was already in the circuit)
--
-- Need to be able to return a list of nodes, in the case of a struct
-- or array

genExp' :: Circuit -> Exp -> OutMonad (Circuit, (Either CircuitCtx [Gr.Node]))
genExp' c exp =
    case exp of
      (BinOp op e1 e2)  -> do i <- nextInt
                              (c1', [gate1]) <- genExp c   e1
                              (c2', [gate2]) <- genExp c1' e2
                              let ctx = mkCtx $ Gate i VoidT (Bin op) [gate1, gate2] [] Nothing
                              return (c2', Left ctx)

      (UnOp op e1)      -> do i <- nextInt
                              (c1', [gate1]) <- genExp c   e1
                              let ctx = mkCtx $ Gate i VoidT (Un op) [gate1] [] Nothing
                              return (c1', Left ctx)

      (EVar var)        -> do gates <- getsVars $
                                       fromJustMsg "CircGen: Lookup EVar" .
                                       maybeLookup var
                              return (c, Right gates)

      (ELit l)          -> do i <- nextInt
                              let ctx = mkCtx (Gate i VoidT (Lit l) [] [] Nothing)
                              return (c, Left ctx)

      -- here we try to update the Typ annotation on the Gate
      -- generated recursively
      (ExpT typ e)      -> do (c', res) <- genExp' c e
                              case res of
                                (Left          (c1, c2, gate,                 c3)) ->
                                    let ctx' = (c1, c2, setGateType typ gate, c3)
                                    in  return (c', Left ctx')
                                (Right node)    ->
                                        return (c', Right node)

      (EStruct str_e (offset,len)) ->
                           do (c', gates) <- genExp c str_e
                              return (c', Right $ take len $ drop offset gates)

      (EStatic e)       -> genExp' c e

      e                 -> error $ "CircGen: unknown expression " << e
-- TODO: EArr
                              
             
-- make an unlabelled edge
uAdj n = ((), n)


-- make a graph Context for this operator, node number and source gates
mkCtx g@(Gate node _ _ srcs _ _) = let ctx = (map uAdj srcs,
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
    in  form_args



--------------------
-- state access functions
--------------------
getsVars f = St.gets $ f . fst

getInt :: OutMonad Int
getInt = St.gets snd

--getVars :: OutMonad [VarTable]
getVars = getsVars id

-- modify the various parts of the state with some function
-- modifyInt f = St.modify (projSnd f)
modifyInt  = St.modify . projSnd
modifyVars = St.modify . projFst


-- update the location of 'var' by some function.
-- it only touches the first vartable where the var is found, not any of the
-- outer scope tables
updateVar f var = modifyVars $ mapOne $ maybeAdjust f var


insertVar var gates = modifyVars $ modtop $ Map.insert var gates

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
fromJustMsg msg Nothing  = error $ "fromJust Nothing: " << msg



instance Show Gate where
    show (Gate i typ op srcs flags doc) = i << (if not (null flags) then "#" << show flags else "") 
                                            << ": "
                                            << op << " [" << typ << "] "
                                            << srcs
                                            << (case doc of
                                                  Just exps  -> " ~" << exps << "~"
                                                  Nothing    -> "")

instance Show GateFlags where
    show Output = "o"

instance Show [GateFlags] where
    show flags = concatMap show flags


instance Show Op where
    show (Bin op) = show op
    show (Un  op) = show op
    show Input = "in"
    show Select   = "sel"
    show (Lit l)        = "Lit " << l


showCct c = Graphviz.graphviz' c
 