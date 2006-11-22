{-# OPTIONS_GHC -fallow-overlapping-instances  -fglasgow-exts #-}

-- code to generate a circuit from an unrolled list of statements,
-- consisiting only of SAss, and SIfElse.

{-
How should non-input arrays be dealt with?
- need to establish a gate for the array
- add a series of initializations to zero the gates? not too
  sensible, best to have that as part of the runtime. but, do add a
  gate to initialize (to zero) the whole array

How is struct initialization dealt with?
- just have runtime rules about default values for uninitialized
  gates?
  - but, each gate needs an input, so will need to connect to the zero
    literal (or whatever) anyway.
  - so, it's actually the compiler which does the rules for this: when
    a value is needed and it's unitialized, pull in from a zero
    literal.
-}

module CircGen (
                Circuit,
                Gate (..),
                Op (..),
                GateFlags (..),
                genCircuit,
                clip_circuit,
                extractInputs,
                showCctGraph,
                CctShow(..),

                testNextInt,
               ) where


import Array    (array, (!))
import Monad    (foldM, mplus, liftM, zipWithM)
import Maybe    (fromJust, isJust, fromMaybe, catMaybes, isNothing)
import List

import Control.Monad.Trans                      (lift)

import Control.Exception                        (assert)


import Data.Graph.Inductive.Graph               ((&))
import qualified Data.Graph.Inductive.Graph     as Gr
import qualified Data.Graph.Inductive.Basic     as GrBas
import qualified Data.Graph.Inductive.Graphviz  as Graphviz
import qualified Data.Graph.Inductive.Tree      as TreeGr
import qualified Data.Graph.Inductive.Query.DFS as GrDFS
import qualified Data.Graph.Inductive.Query.BFS as GrBFS

import qualified Text.PrettyPrint               as PP

import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import qualified Data.Tree                      as Tree
-- import qualified Data.IntSet                    as IS
-- import qualified Data.IntMap                    as IM

import qualified Control.Monad.State            as St

import SashoLib                                 as Lib
import UDraw
-- import Sasho.XDR

import qualified    TreeLib
import qualified    Container                   as Cont
import              Mapping

import Stack                                    (Stack(..), maybeLookup)

import qualified    GraphLib                    as GrLib

import Common (trace,LogPrio(..),logmsg,logProgress,logDebug,logDump,RunFlag(..))

import Intermediate as Im



-- should we generate SPrint gates for print() statements?
cDOPRINT = False

-- should we store ELit gate locations, and only generate one gate per literal?
cSTORE_LITS = False


-- | Which run-time flags are we interested in?
cRELEVANT_RUN_FLAGS = [DoPrint]


-- gate flags
data GateFlags = Output         -- ^ an output gate
               | Terminal       -- ^ a gate which has to be reached, ie not trimmed out of
                                -- ^ the circuit. currently Output and Print gates
    deriving (Eq,Ord
             , Show, Read
             )


-- the gate operations
data Op =
    Bin Im.BinOp
  | Un  Im.UnOp

  -- read from a dynamic array;
  -- inputs: [enable, array, index];
  -- if 'enable' is true, output is
  -- 1: the new array value (or rather pointer, as the runtime will manage the actual
  --    values), and
  -- 2: of the array element type: either a basic type like Int etc, or a list of Slicer
  --    gates which extract the basic components of the complex type in this array
  -- if 'enable' is false, output is
  -- 1: the new array value (probably same as the old one)
  -- 2: NIL
  | ReadDynArray

  -- initialize an array
  -- parameters:
  -- 1) element size in bytes
  -- 2) number of elems
  | InitDynArray Int Integer

  -- update array; inputs = [enable, array, index, val1, val2, ...]; output is
  -- the updated array. The parameters are:
  -- - a slice of where inputs (concatenated) should end up in the array element.
  --
  -- The input will consist of multiple gates in case of a complex
  -- type, and the runtime can just concat the values to get a lump to
  -- write to the (sliced) array location
  --
  -- if 'enable' is false, do not actually write the value.
  | WriteDynArray Im.FieldLoc

  -- an input gate
  | Input

  -- select one of two values based on a test; the input wires are to
  -- be: [test, src_true, src_false]
  | Select


  -- a gate which takes only a part of its input, specified by a
  -- zero-based offset and a length, in bytes;
  -- used after a ReadDynArray, to collect the output of ReadDynArray
  -- into separate gates
  -- for internal use (in Runtime.hs), want to also keep a more high-level interpretation:
  -- offset and length in GateVal's
  | Slicer FieldLoc

  | Lit Im.Lit                  -- a literal

  | Print String                -- ^ a void-type print gate: prints a value preceded by a
                                -- ^ fixed prompt
 deriving (Eq, Show, Read)


--

-- QUESTION: do we include as part of Gate the numbers of the input
-- Gate's? That info should be available in the graph structure, BUT
-- the order of in-edges is important for non-commutative operators,
-- whereas in standard graphs the order of edges is immaterial. For
-- now I do include the input gate numbers
data Gate = Gate {gate_num   :: Int,            -- the gate number
                  gate_typ   :: Typ, -- the output type of the gate
                  gate_op    :: Op,  -- gate operation (+,*, & etc)
                  gate_inputs :: [Int], -- numbers of the input gates, in the
                                -- right order. may do away with this
                                -- if the graph can mainatain
                                -- edge-order
                  gate_depth :: Int, -- depth in nested conditionals
                  gate_flags :: [GateFlags],
                  gate_doc :: GateDoc } -- some documentation, eg. a var name
            deriving (Show, Read)


type GateDoc = [Exp]


--setFlags    newfl g = g {flags=newfl}
--setGateType newt  g = g {typ = newt}


-- NOTE: the gate number is the same as the Node value in the Graph
-- (which is also an Int)
-- helper to make a labelled Node from a Gate
gate2lnode :: Gate -> Gr.LNode Gate
gate2lnode g@(Gate {gate_num=i}) = (i,g)



gateProjNum  f  g@(Gate {gate_num=i})        = g { gate_num    = f i }
gateProjSrcs f  g@(Gate {gate_inputs=is})    = g { gate_inputs = f is }
gateProjDoc  f  g@(Gate {gate_doc=ds})    = g { gate_doc = f ds }
gateProjFlags f g@(Gate {gate_flags=fs})    = g { gate_flags = f fs }

-- needed to lookup a variable's current gate number
-- Structs and direct arrays occupy multiple gates, so need to actually store
-- a list of gate numbers
-- And now, need to place array references in a separate (non-stacked, shared across cond.
-- scopes) table. To this end:
--  - when adding a Var, its type needs to be supplied, so that we can find the component
--    EArr's.
--  - We keep the entire list in the normal (scoped) table, but each entry is annotated:
--    EArr entries have an annotation with the EArr, so can then look it up in the
--    ArrLocTable table.
--    Also need to keep the Var's type, as otherwise we cannot get it in
--    genCondExit.addSelect.
--  - The ArrLocTable maps from EArr (which is Exp) -> Node, and is used for all EArr with
--    a location.
type LocTableVal = (Gr.Node, Maybe LocAnnot)
type LocTable = Map.Map Var ([LocTableVal], Typ)

data LocAnnot = Array Exp       -- this is an EArr, as specified
                deriving Show

type ArrLocTable = Map.Map Exp  -- ^ an EArr expression
                           Gr.Node -- ^ its location.


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

-- | Generate the circuit for these unrolled statements, with the given input parameters.
genCircuit rtFlags type_table stms args =
    let startState      = MyState { loctable    = ([Map.empty], Map.empty),
                                    cond_nodes  = [], -- empty stack
                                    counter     = 0,
                                    typetable   = type_table,
                                    flags       = rtFlags `intersect` cRELEVANT_RUN_FLAGS
                                  }
                          `logDebug`
                          ("genCircuit rtFlags = " ++ show rtFlags)
        -- the main circuit generation step.
        (circ, st)      = St.runState (genCircuitM stms args)
                                      startState
        bad_ctxs        = checkCircuit circ
{-
        _               =   if not $ null bad_ctxs
                            then error ("Bad contexts after circuit generation:\n"
                                        ++ (concat $ intersperse "\n" $ map showCtx bad_ctxs)
                                       )
                            else error "No bad contexts!"
        circ'           = Gr.delNodes (map Gr.node' bad_ctxs) circ
                          `trace` ("Bad contexts after circuit generation:\n"
                                   ++ (concat $ intersperse "\n" $ map showCtx bad_ctxs)
                                  )
-}
        unique_lits_cct = collapse_lit_gates circ
                          `logProgress`
                          ("Initial circuit generation done; circ len="
                           << Gr.noNodes circ
                           -- << showBadCtxs bad_ctxs
                          )
                          `logDump`
                          (let gr_pretty = showCctGraph circ
                           in "The generated graph:\n" ++ gr_pretty
                          )
        
--                                `trace` ("The full circuit: " << show circ)

        bad_ctxs2       = checkCircuit unique_lits_cct
        clipped_circ    = clip_circuit unique_lits_cct
                          `logProgress`
                          ("collapse_lit_gates done; circuit size="
                           << Gr.noNodes unique_lits_cct
                          )
{-
                          `trace` ("Number of Lit gates before trim: "
                                   << numLitGates unique_lits_cct
                                   << "; and without outgoing edges: "
                                   << (length $
                                       GrBas.gsel
                                                ((isLit . Gr.lab') .&& ((== 0) . Gr.outdeg'))
                                                (unique_lits_cct)
                                      )
                                   << showBadCtxs bad_ctxs2
                                  )
-}
        renum_circ      = renumber clipped_circ
                          `logProgress` ("clipped_circ done; Number of Lit gates after trim: "
                                         << numLitGates clipped_circ)
--                                `trace` ("The clipped circuit " << show clipped_circ)
        gate_list       = flatten_circuit renum_circ
--                                `trace` ("The renumbered circuit " << show renum_circ)
        -- expand all Typ fields in the gates, as it helps with printing out the types
        -- (using docTypMachine)
        gate_list'      = map (expandGateTyp type_table) gate_list
    in (renum_circ, gate_list')
           `trace` ("The DFS forest: " -- << genDFSForest renum_circ
                    << "And the flat circuit:\n" << map strShow gate_list')

        where numLitGates   = length . GrBas.gsel (isLit . Gr.lab')
              isLit g       = case gate_op g of (Lit _)   -> True
                                                _         -> False


-- | Check the circuit for consistency, return the list of bad contexts (hopefully empty)
checkCircuit :: Circuit -> [CircuitCtx]
checkCircuit c =    let bads = catMaybes . map checkInvariants . GrLib.contexts $ c
                    in  if null bads
                        then bads
                        else bads {-`trace` (let gr_pretty = showCctGraph c
                                           in  "The bad graph:\n"
                                               << gr_pretty << "\n") -}

-- | Return Nothing if the 'Context' is fine, Just ctx if it has a problem.
checkInvariants ctx@(ins,n,gate,outs)   = if (sort (map snd ins) /= sort (gate_inputs gate) ||
                                              gate_num gate /= n)
                                          then Just ctx
                                          else Nothing

showBadCtxs ctxs = if null ctxs
                   then ""
                   else "Bad circuit contexts:\n"
                            << (concat $ intersperse "\n" $ map showCtxDbg ctxs)

showCtxDbg :: CircuitCtx -> String
showCtxDbg ctx@(ins,n,gate,outs) =
    ("Node " << n
     << "; ins=" << map snd ins
     << "; gate=" << gate
     << "; outs=" << map snd outs)


genDFSForest g = Tree.drawForest $ GrDFS.dffWith' strNode g
    where strNode (_,n,_,_) = show n



-- keep only gates which are reverse-reachable from terminal gates
clip_circuit :: Circuit -> Circuit
clip_circuit c = let c_rev          = {-# SCC "grev" #-} GrBas.grev c
                     out_nodes      = map Gr.node' $
                                      {-# SCC "gsel" #-} GrBas.gsel isTerminal c_rev
                     reach_nodes    = {-# SCC "bfsn" #-} GrBFS.bfsn out_nodes c_rev
                     reach_gr       = keepNodes reach_nodes c
                 in  reach_gr
    where isTerminal = elem Terminal . gate_flags . Gr.lab'



-- renumber the nodes so they are consecutive, in the same order as the original numbering
renumber :: Circuit -> Circuit
renumber g   =  Gr.gmap doRenum g
          -- the returned map (an array) will map from current numbering to consecutive
          -- numbering
    where renumMap                      = let nodes = sort $ Gr.nodes g
                                          in  array (head nodes, last nodes)
                                                    (zip nodes [0..])
                                                    `trace` ("Renum array range=" << head nodes
                                                             << "-" << last nodes
                                                             << "; num nodes=" << Gr.noNodes g)
--                                                    `trace` ("renumber nodes: " << nodes)
          doRenum (ins,node,gate,outs)  = ( map (projSnd renum) ins,
                                            renum node,
                                            gateProjNum renum $
                                               gateProjSrcs (map renum) $ gate,
                                            map (projSnd renum) outs
                                          )
                                          `trace`
                                          ("doRenum ins=" << map snd ins
                                           << "; outs=" << map snd outs
                                           << "; gate num=" << gate_num gate
                                           << "; gate srcs=" << gate_inputs gate
                                          )
          renum node                    = renumMap ! node
                                          `trace` ("renumbering node " << node)
{-
          renumIns g                    = g { gate_inputs = (let renum node = renumMap ! node
                                                                              `trace`
                                                                              ("Renum ins on gate " << g
                                                                               << "; in node=" << node)
                                                             in  map renum $ gate_inputs g
                                                            )
                                            }
-}


flatten_circuit :: Circuit -> [Gate]
flatten_circuit c  = let gates  = GrDFS.topsort' c
                         gates' = ins_to_front gates
                     in  gates'
          -- get the input gates to the front of the list. this partition should not
          -- disturb the topological sort
    where ins_to_front gates =
              let (ins, others) = List.partition ((== Input) . gate_op)
                                                 gates
              in  ins ++ others


-- | Collapse replicated Lit gates to one gate per literal value, and patch up the
-- circuit.
-- No need to remove the discarded Lit gates, they will be removed when the circuit is
-- trimmed.
collapse_lit_gates :: Circuit -> Circuit
collapse_lit_gates g = let (lit_map, replace_map)   = build_maps g
                           toporder                 = GrDFS.topsort g
                       -- NOTE: doing the map in the right order is important. Otherwise
                       -- an edge may be created in the result graph to/from a vertex
                       -- which is not yet in there.
                       in  GrLib.vmap_ordered (patch_ctx replace_map) toporder g
    where -- Build two maps:
          -- lit_map: at what gate is each Lit value? We use the first gate where the lit
          -- value occurs
          -- replace_map: map from current Lit gate numbers, to the new compacted lit gates.
          -- note that using an IntMap for replace_map did not help performance but
          -- worsened by 5% or so.
          build_maps g = foldl add_gate_to_maps (Map.empty, Map.empty) $
                         map Gr.lab' $
                         -- make sure lowest numbered Contexts are first, so they are the
                         -- kept as the gates for each Lit value
                         sortBy (compareWith Gr.node') $
                         GrBas.gsel (isLit . gate_op . Gr.lab') g
          add_gate_to_maps (lit_map, replace_map) g =
              let (Lit l)       = gate_op g
                  mb_last_entry = Mapping.lookup l lit_map
              in  case mb_last_entry of
                    Nothing    -> -- new lit - add it to the lit_map
                                 ( Mapping.insert l (gate_num g) lit_map,
                                   replace_map )
                    Just entry -> -- already have a gate $g$ for this lit,
                                  -- add redirect from this gate number to
                                  -- $g$
                                 ( lit_map,
                                   Mapping.insert (gate_num g)
                                                  entry
                                                  replace_map )
          -- update all the in-edges and gate inputs to point to the selected unique Lit
          -- gates.
          patch_ctx repl (ins,n,g,outs)
                                    = let ins'  = map (projSnd $ do_replace repl) ins
                                          g_ins = map (do_replace repl) $ gate_inputs g
                                          -- outs should not be affected anywhere
--                                          outs'  = map (projSnd $ do_replace repl) outs
                                          -- NOTE: we do not use outs here, and rely on
                                          -- all edges to be added as in-edges of some
                                          -- gate, which should be fine
                                          ctx'  = (ins', n, g {gate_inputs = g_ins}, [])
                                      in  ctx'
                                          `trace`
                                          ("For Lit compaction on node " << n
                                           << " replacing " << map snd ins
                                           << " with " << map snd ins'
                                           << "; ctx'=" ++ showCtxDbg ctx')
          -- replace a key with a value if key is in the map, otherwise just return the key.
          do_replace map x          = fromMaybe x (Mapping.lookup x map)
          isLit (Lit _) = True
          isLit _       = False
                                          


expandGateTyp :: TypeTable -> Gate -> Gate
expandGateTyp typetable g@(Gate { gate_typ = t })    =
    let t' = Im.expandType' typetable [Im.DoAll] t
    in
      g { gate_typ = t'
--                     `trace` ("expandGateTyp " << t << " -> " << t')
        }

    
-- | remove the nodes (and associate edges) not in 'keeps' from 'g'.
keepNodes :: Gr.Graph gr => [Gr.Node] -> gr a b -> gr a b
-- profiling showed that the N^2 (\\) list difference operation was taking a lot of time!
-- So use a Set to do the difference operation.
keepNodes keeps g = let dels = Set.toList $
                               Set.difference (Set.fromList $ Gr.nodes g) (Set.fromList keeps)
                    in  Gr.delNodes dels g



-- | the stateful computation to generate the whole circuit.
genCircuitM :: [Stm] -> [TypedName] -> OutMonad Circuit
genCircuitM stms args =
    do input_gates_cct  <- genInputs args
       start_cct        <- initCondNodes input_gates_cct
       foldM genStm start_cct stms


-- | Generate the initial graph for the inputs; it will consist of several Input gates,
-- and no edges.
genInputs :: [TypedName] -> OutMonad Circuit
genInputs names     = do gatess     <- mapM createInputGates names
                         return     (Gr.mkGraph (map gate2lnode (concat gatess)) [])

-- | Initialize the stack of condition nodes, by adding a "True" literal for the
-- unconditional area
-- returns the circuit with the new condition node.
initCondNodes cct   = do i                  <- nextInt
                         d                  <- getDepth
                         let rootCondCtx    = mkCtx $ Gate i
                                                           BoolT
                                                           (Lit $ LBool True)
                                                           []
                                                           d
                                                           [] []
                         pushCondNode i
                         return $ rootCondCtx & cct
                                                           
                                                           



-- create the input gates for this argument to main(), and add the vars
-- to the top-level LocTable
-- will make this return a [Gate], for when we deal with arrays etc
--
-- we'll only insert mappings into the LocTable at the top level, not
-- in recursive calls (then we'd be inserting struct field names as
-- actual vars)

-- also have some gymnastics to generate full annotations for the gates
createInputGates :: TypedName -> OutMonad [Gate]
createInputGates (name, typ) =
 do typ'    <- Im.expandType [DoFields, DoTypeDefs] typ
    createInputGates' Nothing Nothing (name, typ')

   where
    createInputGates' mb_parent_exp mb_field_num (name, typ) =
         do let var         = add_vflags [FormalParam] (VSimple name)
            -- NOTE: the ExpT annotation here is important so we can later
            -- print actual field names, and not just field numbers; see
            -- Intermediate.docExp
                this_exp    = ExpT typ $ maybe (EVar var)
                                               (\e -> EStruct
                                                      e
                                                      (fromJustMsg "createInputGates"
                                                                   mb_field_num)
                                               )
                                               mb_parent_exp
            case typ of
              (StructT (fields,_))
                -> do gatess <- zipWithM (createInputGates' (Just this_exp))
                                         (map Just [0..])
                                         fields
                      let gates = concat gatess
                          is    = map gate_num gates
                      -- if we have a parent expression, must be a part of a parent
                      -- struct, so do not add gate locations.
                      if isNothing mb_parent_exp then setVarLocs (var,typ) is
                                                 else return ()
                      return gates
                      
{-
        (SimpleT tname)
                -> let typ' = fromJust $ Map.lookup tname type_table
                   in  createInputGates (name, typ')
-}


        -- IntT, BoolT, ArrayT (for dynamic arrays):
              _   -> do i <- nextInt
                        if isNothing mb_parent_exp
                          then setVarLocs (var,typ) [i]
                                   `trace`
                                   ("inserting input var " << var << " into LocTable")
                          else return ()
                        gate <- mkGate i typ this_exp
                        return [gate]

    mkGate i typ doc_exp = do typ_full <- Im.expandType [DoAll] typ
                              return $ Gate i
                                            typ_full
                                            Input
                                            []
                                            0
                                            []
                                            [doc_exp]





-- this just adds an obvious dummy entry of the correct size (ie. number of graph nodes)
-- in the loc table for this lval.
-- we only need to do it for structs which are variables, and not
-- expressions (ie. parts of other complex types)
-- PRE: the lval does not have a top-level ExpT
genComplexInit (lval, t) size =
    case lval of
      (EVar var)        -> setVarLocs (var,t) (replicate size (-12345678))
                            `logDebug`
                            ("genComplexInit setting dummy locs for var " << var)
      _                 -> return ()
                            `logDebug`
                            ("genComplexInit on non-var lval " << lval)




-- get the gates corresponding to expression 'e', if necessary adding
-- new gates to the circuit, and add e_doc as the last annotation of
-- that gate
-- returns the new circuit, and the gates where 'e' is
-- also have the gates pass through a hook which may update them, eg. add
-- flags
genExpWithDoc :: Circuit ->
                 Maybe (Gate -> Gate) -> -- ^ A hook to apply to a gate generated for the
                                         -- expression
                 Exp ->         -- ^ The expression to translate
                 Exp ->         -- ^ The doc expression
                 OutMonad (Circuit, [Gr.Node])
genExpWithDoc c ghook e e_doc =
    do (c', res) <- genExp' c e
       let (c'', nodes) = case res of
                            (Left [ctx]) ->
                            -- we got Contexts back, so
                            -- add the doc to the gate, and apply the
                            -- hook
                                let ctx'    = processCtx ctx
                                in  (ctx' & c', [Gr.node' ctx'])

                            (Left ctxs) ->
                                -- FIXME: adding the same doc to all the gates
                                let ctxs'   = map processCtx ctxs
                                in ( foldl (flip (&)) c' ctxs',
                                     map Gr.node' ctxs' )
                                 `trace` "genExpWithDoc on multiple contexts"

                            (Right nodes) ->
                                -- no new gate generated, but we will
                                -- update the gate doc and apply the hook
                                let c'' = foldl (updateLabel processGate)
                                                c'
                                                nodes

                                in  (c'', nodes)
       return (c'', nodes)

    where addGateDoc exp    = gateProjDoc $ push $ stripExpT exp
          processGate g     = addGateDoc e_doc $ maybeApply ghook g
          processCtx        = tup4_proj_3 processGate


-- update the label of a graph Context
updateCtxLab new_label = tup4_proj_3 (const new_label)



-- get the offset of an expression, relative to the simple variable
-- under which this expression is, eg. for a struct expression x.y.z we want the
-- offset of field 'y.z' under struct 'x'; also return what is the root
-- variable, 'x' in this example
-- the 'loc_extr' parameter is a function which specifies which struct
-- locations we use, primitive type (getStrTLocs) or byte (getStrTByteLocs)

getRootvarOffset :: (TypeTableMonad m)                  =>
                    ( ([TypedName], [FieldLoc]) -- the params of a StructT
                          -> [(Int,Int)] )              ->
                    Exp                                 ->
                    m (Var, Typ, Int)
getRootvarOffset loc_extr exp =
    do let rec = getRootvarOffset loc_extr
       case exp of
        (ExpT t (EVar v))   -> return (v, t, 0)
        (EVar v)            -> error
                               ("getRootvarOffset got an EVar " << exp
                                << " without an ExpT annotation!")
        (EStruct str_e idx) -> do  (v, t, o)  <- rec str_e
                                   fld_info   <- getStrTParams str_e
                                   let locs   = loc_extr fld_info
                                       preceding  = sum $
                                                    map snd $
                                                    take idx locs
                                   return (v, t, o + preceding)
        (ExpT t e)          -> rec e
        (EArr arr_e idx_e)  -> rec arr_e
        e                   -> error $
                               "CircGen: getRootvarOffset: invalid lval "
                               << e


-- get the field parameters of a StructT, from the expression carrying the struct
getStrTParams      (ExpT t _)   = do (StructT field_params)  <- Im.expandType [DoTypeDefs] t
                                     return field_params
getStrTParams     e             = error $
                                  "getStrTParams: unexpected expression "
                                  << e


genStm :: Circuit -> Stm -> OutMonad Circuit
genStm circ stm =
    let bad_ctxs = checkCircuit circ
    in 
     case stm `logDump` ("genStm of " << stm) 
     {- << ": "
        << showBadCtxs (checkCircuit circ)) -}
     of

      -- this is a message from the typechecker that a Struct or such
      -- will be coming along just now.
      -- this case should be before the "gen-stm-ass-var" case, or an EStructInit may end
      -- up in genExp which doesn't know how to treat it.
      (SAss (ExpT _ lval) (ExpT t (EStructInit size))) ->
          do genComplexInit (lval, t) size
              `logDebug`
              ("calling genComplexInit, with stm=" << stm)
             return circ

      -- assignment to a variable.
      s@(SAss (ExpT lval_t lval@(EVar var)) exp) ->
          {-# SCC "gen-stm-ass-var" #-}
          do circ'      <- checkOutputVars circ var Nothing
                               {- `trace` ("genStm " << s <<
                                        "; var=" << var <<
                                        "; loctable=" << var_table) -}
             (c',nodes) <- genExpWithDoc circ'
                                          (addOutputFlag var)
                                          exp
                                          lval
             setVarLocs (var, lval_t) nodes
             return c'


      -- do away with lval type annotations for now
      -- NOTE: the cases above do actually use the ExpT annotation, so cannot be below
      -- this case.
      (SAss (ExpT _ lval) val) -> genStm circ (SAss lval val)



      s@(SAss lval@(EStruct str_e idx)
              val)                              ->
          {-# SCC "stm-ass-estruct" #-}
          do type_table    <- getTypeTable
             (_,locs)      <- getStrTParams str_e
             let (off,len)  = valloc $ locs !! idx
             (rv,rv_t,lval_off) <- getRootvarOffset getStrTLocs lval
             c2             <- checkOutputVars circ rv (Just (lval_off,len))
             (c3, gates)    <- genExpWithDoc c2
                                             (addOutputFlag rv)
                                             val
                                             lval
             arr_info       <- extractEArr lval
             case arr_info of
               Nothing  -> {-# SCC "no-lval-array" #-}
                           -- just update the lval location(s)
                           do spliceVarLocs (lval_off,len) (rv,rv_t)
                                            (gates
                                             `logDebug`
                                             ("stm-ass-estruct no-lval-array splice = "
                                              << show (lval_off,len) << "; rv = " << rv
                                              << "; stm = " << stm << "; gates=" << gates)
                                            )
                              return c3
               Just ((EArr arr_e idx_e), arr_off, locs)
                                    -> -- need to generate a WriteDynArray gate!
                                       -- eg: for x.y[i].z, we will:
                                       -- - add a WriteDynArray gate for x.y, limited to .z
                                       -- - update the gate location for x.y
                                       {-# SCC "have-lval-array" #-}
                                       do (c4, [arr_n]) <- genExp c3 arr_e
                                          (c5, [idx_n]) <- genExp c4 idx_e
                                          condNode      <- getCondNode
                                          depth         <- getDepth
                                          i             <- nextInt
                                          let prep_slice locs   = (fst $ head locs,
                                                                   sum $ map snd locs)
                                              -- get the slice parameters from the field
                                              -- location info given by extractEArr above
                                              slice         = Im.FieldLoc
                                                              { byteloc = prep_slice $
                                                                          map Im.byteloc locs,
                                                                valloc  = prep_slice $
                                                                          map Im.valloc locs }
                                              (ExpT arr_t _)    = arr_e
                                              -- Adding the index expression as an
                                              -- annotation, so we can tell later if it
                                              -- was static or not.
                                              ctx        = mkCtx $
                                                           Gate i
                                                                arr_t
                                                                (WriteDynArray slice)
                                                                ([condNode, arr_n, idx_n]
                                                                 ++ gates)
                                                                depth
                                                                [] [idx_e]
                                          spliceVarLocs (arr_off,1) (rv,rv_t) [i] 
                                                        `trace`
                                                        ("WriteDynArray: rv = " << rv
                                                         << "; arr_off=" << arr_off)
                                          return $ ctx & c5


      -- quite similar to the above. really need to structure this better
      s@(SAss lval@(EArr arr_e idx_e) val) ->
         {-# SCC "stm-ass-earr" #-}
          do (rv,rv_t,off)      <- getRootvarOffset getStrTLocs arr_e
             -- generate gates for the rval
             (c3, gates)        <- genExpWithDoc circ
                                                 (addOutputFlag rv)
                                                 val
                                                 lval
             -- gate for the array lval
             (c4, [arr_n])      <- genExp c3 arr_e
             -- gate for the index
             (c5, [idx_n])      <- genExp c4 idx_e
             depth              <- getDepth
             cond_node          <- getCondNode
             i                  <- nextInt
             let (ExpT arr_t _)  = arr_e
                 ctx             = {-# SCC "mkCtx" #-}
                                   (mkCtx $ Gate i
                                                 arr_t
                                                 -- NOTE: writing -1 here to mean "the end"
                                                 (WriteDynArray $ Im.FieldLoc (0,-1) (0,-1))
                                                 ([cond_node, arr_n, idx_n] ++ gates)
                                                 depth
                                                 [] [])
             spliceVarLocs (off,1) (rv,rv_t) [i]
             return $ {-# SCC "ctx-&-c5" #-} (ctx & c5)
                        `logDump`
                        ("SAss to EArr: \"" ++ show stm ++
                         "\n; inserting " ++ show ctx)




      -- NOTE: after HoistStm.hs, all conditional tests are EVar
      (SIfElse test@(ExpT _ (EVar testVar))
               (locs1, stms1)
               (locs2, stms2))  ->
          {-# SCC "stm-ifelse" #-}
          do [testGate]         <- lookupVarLocs testVar >>==
                                   fromJustMsg ("Conditional var " << testVar
                                                << " not found, within " << stm)
             -- add in the enable gates
             (circ', t_en_node, f_en_node)  <- prepareCondNodes circ testGate

             -- do the recursive generation for both branches, and
             -- save the resulting var scopes
             pushCondNode t_en_node
             pushScope
             circ1'             <- foldM genStm circ' stms1
             ifScope            <- popScope
             popCondNode

             pushCondNode f_en_node
             pushScope
             circ2'             <- foldM genStm circ1' stms2
             elseScope          <- popScope
             popCondNode

             -- grab the parent scope
             parentScope        <- getsLocs fst
             -- generate the conditional exit gates
             circ''             <- genCondExit testGate
                                               circ2'
                                               (parentScope, ifScope, elseScope)
                                               (locs1, locs2)
             return circ''

      -- we will wire all the variables in x to go through this gate
      -- a bit of a hassle as there may be one or more gates feeding into here (in case of
      -- a struct). So, add slice gates
      (SPrint prompt xs)  -> do flags   <- getFlags
                                if not $ elem DoPrint flags
                                 then return circ
                                 else
                                   do i                 <- nextInt
                                      depth             <- getDepth

                                      -- generate parameters for each expression separately.
                                      (circs,
                                       x_gates's,
                                       ts's,
                                       slice_is's,
                                       locs's)          <- scanM (doSPrintExp prompt . \(f1,_,_,_,_)->f1)
                                                                 (circ,[],[],[],[])
                                                                 xs
                                                              >>== tail >>== unzip5
                                      -- now need to shift the offsets of the slicers,
                                      -- as each was made assuming it starts at 0
                                      -- this is quite awkward, as we need to dig inside
                                      -- the FieldLoc's and change the GateVal and byte
                                      -- offsets only
                                      -- 'extrLen' is to get the length out of the
                                      -- FieldLoc struct
                                      -- 'proj' is to project the addition onto the
                                      -- correct offset field of the FieldLoc
                                      let shiftlocs extrLen proj locss
                                                     = let lens     = map (extrLen . last) locss
                                                           shifts   = runSumFrom0 lens
                                                       in zipWith (\shift locs ->
                                                                       map (proj (+ shift))
                                                                           locs)
                                                                  shifts
                                                                  locss
                                          locs's2           = shiftlocs (snd . Im.valloc)
                                                                        proj_FL_val_off
                                                                        locs's
                                                              `trace`
                                                              ("Stm SPrint: locs's = " << locs's)
                                          locs's3           = shiftlocs (snd . Im.byteloc)
                                                                        proj_FL_byte_off
                                                                        locs's2

                                          slicer_ctxs = [mkCtx $
                                                         Gate si t (Slicer loc) [i] depth [] []
                                                              | si      <- concat slice_is's
                                                              | t       <- concat ts's
                                                              | loc     <- concat locs's3]

                                      -- and the actual print context
                                      -- NOTE: can't really give it a type, as it can return
                                      -- several values, noone should access it directly
                                      -- anyway, just through the slicers.
                                      let ctx           = mkCtx $ Gate i
                                                                       VoidT
                                                                       (Print prompt)
                                                                       (concat x_gates's)
                                                                       depth
                                                                       []
                                                                       []

                                      return $ addCtxs (last circs) (ctx:slicer_ctxs)
                                          {-   `trace` 
                                              -- FIXME: add StreamShow instances for tuples...
                                               ("On SPrint, adding contexts " << (ctx:slicer_ctxs)
                                                 << ", with current circuit " << show (last circs))
                                                 ) -}



      s -> error $ "CircGen: unrecognized statement: " ++ (show s)

    where addOutputFlag var =
              case getVarFlags var of
                []      -> Nothing
                flags   -> Just (gateProjFlags (\fs -> fs `union` flags))
                           

-- | Make a gates to be the two condition nodes for this conditional scope - the
-- true-branch enable node is an AND of
-- the current condition, and the next higher cond node; the false-branch enable is 
-- (parent AND (NOT cond))
-- returns the circuit with the enable gates added in, as well as the two gate addresses.
prepareCondNodes cct this_cond
                            = do parentNode     <- getCondNode
                                 [nCondNode, tNode, fNode]  <- replicateM 3 nextInt
                                 d              <- getDepth
                                 let not_cond_ctx =
                                                  mkCtx $ Gate nCondNode
                                                               BoolT
                                                               (Un Im.Not)
                                                               [this_cond]
                                                               d
                                                               [] []
                                     true_ctx   = mkCtx $ Gate tNode
                                                               BoolT
                                                               (Bin Im.And)
                                                               [parentNode, this_cond]
                                                               d
                                                               [] []
                                     false_ctx  = mkCtx $ Gate fNode
                                                               BoolT
                                                               (Bin Im.And)
                                                               [parentNode, nCondNode]
                                                               d
                                                               [] []
                                 return (addCtxs cct [not_cond_ctx, true_ctx, false_ctx],
                                         tNode,
                                         fNode)

-- what flags to attach to a gate for this 'var'.
-- if it is called "main" and is a function return variable, it needs an Output flag.
getVarFlags var = case (elem RetVar $ vflags var, varName var) of
                    (True, cMAINNAME)   -> [Output, Terminal]
                    _                   -> []


-- do most of the work for a single expression in an SPrint
doSPrintExp prompt circ e =
    do (circ', x_ns)     <- genExp circ e
       slice_is          <- replicateM (length x_ns) nextInt
       let (t,var) = case (e `trace` "SPrint e = " << e) of
                       (ExpT t (EVar v))   -> (t,v)
                       (ExpT t _)          -> error ("SPrint `" << prompt
                                                     << "' got a non-var: "
                                                     << e
                                                     << " of type "
                                                     << t)
       t_full <- Im.expandType [DoAll] t
       let tinfo@(ts, locs)      = case t_full of
                                     (StructT (tn's, locs))
                                         -> (map snd tn's,
                                             locs)
                                     _
                                         -> let blen = Im.typeLength Im.tblen
                                                                     t_full
                                            in ([t_full],
                                                [Im.FieldLoc (0,blen)
                                                             (0,1)])
       setVarLocs (var, t) slice_is
       return (circ', x_ns, ts, slice_is, locs)



-- extract an array sub-expression from the given Exp
-- also return the list of field-locations (in bytes) of the array element type
-- which are covered by the whole expression
-- and also return the offset of the array expression under its root variable.
-- by example, say we have an expression x.y[i].z.v
-- then, the outputs are
-- ( the array expression x.y[i],
--   the offset of .y under x,
--   the SliceAddr's of .z.v under x.y[i]
-- )
-- return Nothing if there is no array subexpression
extractEArr :: (TypeTableMonad m) => Exp -> m ( Maybe (Exp,
                                                       Int,
                                                       [Im.FieldLoc])
                                              )
extractEArr = runMaybeT . extractEArr'

-- the typechecker actually inferred a more general type on its own...
extractEArr'    :: (TypeTableMonad m) => Exp -> MaybeT m (Exp,
                                                          Int,
                                                          [Im.FieldLoc])
extractEArr' (ExpT elem_t exp@(EArr arr_e idx_e)) =
    do  (locs,_)    <- lift $ getTypLocs elem_t
        -- here we need the offset in words (int/bool)
        (rv,_,off)  <- lift $ getRootvarOffset getStrTLocs arr_e
        return (exp, off, locs )

-- get a recursive answer and return just the slice of this field (idx)
extractEArr' e@(EStruct str_e idx) =
              do (arr_exp,arr_off,sublocs) <- extractEArr' str_e
                 -- this struct's field locations
                 (_,locs)                  <- lift $ getStrTParams str_e
                 -- pick out the location (in words) for this field
                 let (off,len)              = Im.valloc $ locs !! idx
                 return (arr_exp,
                         arr_off,
                         (take len $ drop off sublocs))
                            `logDebug`
                                ("extractEArr' (" << e << ")" <<
                                 "; sublocs=" << sublocs <<
                                 "; off=" << off <<
                                 "; len=" << len)

extractEArr' (ExpT _ e) = extractEArr' e

-- if we hit a primitive expression, there's no array in the exp. This ends up returning
-- Nothing, not causing a runtime error.
extractEArr'   e        = fail ""




-- see if this var is an output var, and if so remove the Output flag
-- on its current gates
-- Called when the location of a var is about to be updated
-- 
-- optionally an offset and length to limit the flag removal to some
-- of the gates, in case only part of a complex output var is being
-- modified.
checkOutputVars :: Circuit -> Var -> Maybe (Int,Int) -> OutMonad Circuit
checkOutputVars c var mb_gate_loc
    | strip_var var == VSimple cMAINNAME =
        -- remove the output flags there
        do mb_vgates    <- lookupVarLocs var
           case mb_vgates of
             Nothing        -> return c
             Just vgates    ->
                 -- take a slice of the gates if mb_gate_loc is not Nothing
                 do let vgates' = maybe vgates
                                        (\(off,len) -> take len $ drop off vgates)
                                        mb_gate_loc
                        -- and update the Gate flags on those gates
                        -- FIXME: for now we just strip the flags, which may be excessive
                        -- when more flags are introduced.
                        -- 
                        -- FIXME: we get non-existant node numbers passed (with number
                        -- -12345678), for non-initialized struct fields. The filter is a
                        -- HACK around this
                        c' = rmOutputFlag c (filter (/= -12345678) vgates')
                    return c'
    | otherwise =
        return c `logDebug` ("checkOutputVars non-matching var: " << var)


-- | remove the Output flag on the gate at this Node, if it is present
-- FIXME: removes both Output and Terminal flags for now
rmOutputFlag :: Circuit -> [Gr.Node] -> Circuit
rmOutputFlag c ns = foldl (updateLabel $ gateProjFlags (\fs -> fs \\ [Output,Terminal]))
                          c
                          ns


-- update the label for a given node in a graph, if this node is present
-- NOTE: strict evaluation
-- had a big space leak here, when using a table of Lit's to reduce the number of Lit
-- gates during circuit generation. The connection between ELit handling, and this
-- function is unclear, but the profiler clearly pointed to a space leak here. Hence all
-- the strictness annotations, which did not help.
updateLabel :: (Gr.DynGraph gr, Eq b) => (a -> a) -> gr a b -> Gr.Node -> gr a b
updateLabel f gr node = let (mctx,gr')      = {-# SCC "#extract" #-}
                                              strictEval $
                                                       Gr.match ({-# SCC "#node" #-} node)
                                                                ({-# SCC "#gr"   #-} gr)
                            ctx             = fromJustMsg ("updateLabel " << node)
                                                          mctx
                            ctx'            = (tup4_proj_3 (strictEval f) $! ctx)
                            g_out           = ctx' & (strictEval gr')
                        in {-# SCC "re-insert" #-}
                          -- make sure that the modified context we re-inserted is the
                          -- same as the original, except the label
                          assert (let (mctx'',_)        = Gr.match node g_out
                                      ctx''             = fromJustMsg ("updateLabel assert") mctx''
                                      (is,n,_,os)       = ctx
                                      (is',n',_,os')    = ctx''
                                  in  (is,n,os) == (is',n',os')
                                 )
                                 g_out
{-                          
                        in  case id $! mctx of
                              Nothing   -> {-# SCC "ret-id" #-}     gr
                              Just ctx  -> {-# SCC "ret-new-gr" #-} (tup4_proj_3 f ctx) &
                                                                    (strictEval gr')
-}




-- generate the gates needed when exiting a conditional block---to
-- conditionally update free variables updated in this scope
--
-- NOTE: the vars in the locals VarSets (ifLocalss and elseLocalss)
-- are without scopes
genCondExit :: (Cont.Container c Var) =>
               Gr.Node          -- ^ Number of the gate with the condition test value
            -> Circuit          -- ^ Starting circuit
            -> ([LocTable],     -- ^ The parent variable scope
                LocTable,       -- ^ Var scope from the true-branch
                LocTable)       -- ^ Var scope from the false-branch
            -> (c,              -- ^ all the variables declared in the true branch
                c)              -- ^ all the variables declared in the false branch
            -> OutMonad Circuit -- ^ The circuit with all the Select gates added in.
genCondExit testGate            
            circ                
            (parentScope,       -- ^ The parent 
             ifScope,
             elseScope)
            (ifLocals, elseLocals) =
    let vars    = List.nub $
                  filter nonLocal $
                  map fst $     -- get just the variables.
                  concatMap Map.toList [ifScope, elseScope]
        sources = map varSources vars
--                  `trace` ("non-local scope vars: " << vars)
        -- make sure to not give vars with empty source lists to addSelect (eg. a struct
        -- with all array elements
        select_args = [ (v, (gs_true, gs_false))
                            | (v, (gs_true, gs_false)) <- zip vars sources , not $ null gs_true
                      ]
    in  foldM addSelect circ select_args
        `logDump`
        ("genCondExit select_args = " << select_args)

    where -- a var is non-local if was not declared in this scope,
          -- *and* it appears in the parent scope (needed in the case of
          -- generated vars)
          -- FIXME: do generated vars ever have to be selected? seems not! they are
          -- intrinsically very local in their usage, and so do not need to persist across
          -- scopes
          nonLocal var = (not $ any (Cont.member (stripScope var))
                                    [ifLocals, elseLocals])
                         && (isJust $ maybeLookup var parentScope)
                         && (notTemp var)

          notTemp (VTemp _) = False
          notTemp _         = True

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
                           in  assert (length gates_true == length gates_false)
                                      out
--                                   `trace` ("varSources for " << var << ": "
--                                            << out)
          -- find the gates for a var, looking in the given scope stack
          -- keep just LocTable entries with a Nothing annotation (ie. not arrays)
          -- WARNING: using the annotation details here is not good, it should be exposed
          -- just in 'getVarLocs' etc.
          gates var scopes = let leaves = fst $ fromJustMsg "genCondExit::gates" $
                                          maybeLookup var scopes
                                 ns     = [ n | (n, Nothing) <- leaves ]
                             in  ns
                                   `logDump`
                                   ("genCondExit.gates(" << var
                                    << ") has scopes=" << show scopes
                                    << "; ns -> " << ns << ";"
                                   )
                                                         

          -- add Select gates for a free variable, and update its
          -- wire locations, to the new Select gates
          -- need multiple select gates if it's a struct, but in this
          -- case we add Select only for the gates which were actually
          -- updated in this scope
          -- PRE: the gates_true' and gates_false' inputs must be the same lenght, and not
          -- null; the type of the var is not an array type
          -- this function is quite nasty!
          addSelect c (var, in_gates@(gates_true', gates_false'))
              | assert (length gates_true' == length gates_false' &&
                        not (null gates_true'))
                       True
              = do let changed_gates    = filter (\(x,y) -> x /= y) $ uncurry zip in_gates
                   typ          <- lookupVarTyp var
                   ts           <- assert (not (arrTyp typ))
                                   mapM (getSelType c) changed_gates
                   -- get the right number of new int's
                   is           <- replicateM (length ts) nextInt
                   let ctxs      = zipWith3 (mkCtx' var) is ts changed_gates
                       new_gates = foo (uncurry zip in_gates) is
                   -- remove Output flag on var's current gates (which will feed into
                   -- Select gates) if flag is present.
                   let c2       = rmOutputFlag c $
                                  -- concat all the elements of the list of pairs
                                  foldr (\(a,b) l -> (a:b:l)) [] changed_gates
                   setScalarLocs (var,typ) new_gates
                     `logDump`
                     ("addSelect, var= " << var
                      << "; in_gates = " << in_gates
                      << "; typ = " << typ
                      << "; new_gates = " << new_gates
                     )

                   -- work all the new Contexts into circuit c
                   return $ addCtxs c2 ctxs

          -- make the Select Context, including gate flags if needed.
          mkCtx' var i t (true_gate,false_gate) =
              let src_gates = [testGate, true_gate, false_gate]
                  depth     = (length parentScope) - 1
                  flags     = getVarFlags var
                  doc       = EVar var -- annotate gate with the variable name
              in  mkCtx (Gate i t Select src_gates depth flags [doc])

          arrTyp (ArrayT _ _)   = True
          arrTyp _              = False
          


-- take a list of pairs, and where a pair is equal, pass on that value, but
-- where they're not equal, use the next value from the second list.
-- ideally, the number  of non-equal pairs should be the same as the lenght of
-- the replacement list
foo :: (Eq a) => [(a,a)] ->    [a] -> [a]
foo    ((x,y):xys)  rs
    | x == y            = (x : foo xys rs)
    | otherwise         = case rs of (r:rs') -> (r : foo xys rs')
                                     []     -> error "CircGen.foo ran out of replacement values!"
foo []  (r:rs)  = error "CircGen.foo had replacement values left over!"
foo []  []      = []



-- add a list of contexts (ie. gates) to a circuit, return the new circuit
addCtxs circ ctxs = foldl (flip (&)) circ ctxs



-- figure out the type of a Select gate, based on the type of its two
-- inputs
getSelType gr (node1, node2)
    = do let gates = map (fromJustMsg "CircGen::getSelType" . Gr.lab gr) [node1, node2]
         -- expanding types into a canonical form
         types <- mapM (Im.expandType [DoTypeDefs]) $ map gate_typ gates
         case types
--                  `trace` ("getSelType of gates " << gates)
                  of
                    [Im.BoolT          , Im.BoolT          ]    -> return Im.BoolT
                    [Im.IntT i1_e      , Im.IntT i2_e      ]    ->
                        do let [i1, i2] = map Im.evalStaticOrDie [i1_e, i2_e]
                           return $ Im.IntT $ Im.lint (max i1 i2)

                    [a1@(Im.ArrayT _ _), a2@(Im.ArrayT _ _)]    -> return a1
                                                                     `trace` ("getSelType got array types "
                                                                              << a1 << " and " << a2)
                    [t1                , t2                ]    ->
                        error ("getSelType got unexpected inputs of type "
                               << t1 << " and " << t2)



-- adds the needed gates to the circuit (if any), and returns at which
-- gate/node numbers the result is
genExp :: Circuit -> Exp -> OutMonad (Circuit, [Gr.Node])
genExp c e = do (c', res) <- genExp' c e
                case res of
                  -- extend the graph with the new contexts, and return it
                  (Left ctxs)       -> return ( foldl (flip (&)) c' ctxs,
                                                map Gr.node' ctxs )
                  -- just pass on the graph and node
                  (Right gateNums)  -> return (c',
                                               gateNums)
                                          `trace`
                                       ("genExp " << e << " returning nodes " << gateNums)


-- this one is a little nasty - it returns the expanded circuit, and:
-- Left:    the Context for this expression, which can be processed and
--          added to the circuit by the caller, or
-- Right:   the gate number where this
--          expression can be found (in case it was already in the circuit)
--
-- In both cases, need to be able to return a list of nodes, in the case of a struct
-- or array
--
-- also need to be able to return a list of newly generated Contexts,
-- for a ReadDynArray gate with all its following Slicer gates

genExp' :: Circuit -> Exp -> OutMonad (Circuit, (Either
                                                  [CircuitCtx]
                                                  [Gr.Node]))
genExp' c exp =
 do depth <- getDepth
    case exp
--          `trace` ("genExp' " << exp)
             of
      (BinOp op e1 e2)  -> do let genOperand circ opExp =
                                      do (c1, gates1) <- genExp circ opExp
                                         let gate1 = case gates1 of
                                                       [g1]   -> g1
                                                       _      -> error ("BinOp arg " << opExp <<
                                                                        " got (/= 1) gates: " <<
                                                                        gates1)
                                         return (c1, gate1)
                              (c1, gate1)   <- genOperand c e1
                              (c2, gate2)   <- genOperand c1 e2

                              -- NOTE: the VoidT type annotation used here is replaced
                              -- when the ExpT enclosing this BinOp is handled.
                              i             <- nextInt
                              let ctx       = mkCtx $ Gate i VoidT (Bin op) [gate1, gate2] depth [] []
                              return (c2, Left [ctx])

      (UnOp op e1)      -> do (c1, [gate1]) <- genExp c   e1
                              i             <- nextInt
                              let ctx       = mkCtx $ Gate i VoidT (Un op) [gate1] depth [] []
                              return (c1, Left [ctx])

      (EVar var)        -> do gates     <- lookupVarLocs var >>==
                                           fromJustMsg ("Variable not found, within exp " << exp)

                              return (c, Right gates)
                                     `logDebug`
                                     ("Found gates " << gates << " for var " << var)
{-
                                         `trace`
                                         ("genExp' EVar " << var << ", loctable=" << show var_table <<
                                          " -> " << gates)
-}


      (ELit l)        -> {-# SCC "Exp-ELit" #-}
                           do i         <- nextInt
                              let ctx   = mkCtx (Gate i (IntT 32) (Lit l) [] depth [] [])
                              return (c, Left [ctx])                           


      -- here we try to update the Typ annotation on the Gate
      -- generated recursively
      (ExpT typ e)      -> do (c', res) <- genExp' c e
                              case res of
                                (Left          [(c1, c2, gate,                 c3)]) ->
                                    let ctx' =  (c1, c2, gate { gate_typ = typ }, c3)
                                    in  return (c', Left [ctx'])
                                -- FIXME: can't deal with multiple contexts
                                -- for now
                                (Left ctxs) ->
                                    return (c', res)
                                (Right node)    ->
                                    return (c', res)

      (EStruct str_e idx) ->
                           do (c', gates)       <- genExp c str_e
                              (_,locs)          <- getStrTParams str_e
                                                   `logDebug`
                                                   ("genExp' EStruct " << exp
                                                    << " struct gates = " << gates)
                              let (off,len)      = (Im.valloc $ locs !! idx)
                                                   `logDebug`
                                                   ("genExp' EStruct " << exp << " locs = " << locs)
                                                   
                              return (c', Right $ take len $ drop off gates)

      (EArr arr_e idx_e) ->
                           do (c1, arr_ns)  <- genExp c arr_e
{-
                                               `trace` ("Circuit before array gate generated:"
                                                        << c)
-}
                              let arr_n      = case arr_ns of
                                                 [arr_n] -> arr_n
                                                 _       ->
                                                     error ("Array " << arr_e <<
                                                            " should get one wire but got: "
                                                            << arr_ns
                                                            {- << "; circuit at error: "
                                                            ++ showCct c1 -}
                                                           )
                              (c2, [idx_n])        <- genExp c1 idx_e
                              readarr_n            <- nextInt
                              depth                <- getDepth
                              cond_node             <- getCondNode
                              -- get the array type and the element type, from the array
                              -- expression annotations.

                              -- NOTE: we do not attach the array type to the ReadDynArray
                              -- gate; trying to see what happens if we do.
                              --
                              -- Add the index expression as an annotation, hopefully in
                              -- an EStatic if it is static.
                              let (ExpT arr_t@(ArrayT elem_t _) _)   = arr_e
                                  readarr_ctx       = mkCtx (Gate readarr_n
                                                                  arr_t
                                                                  ReadDynArray
                                                                  [cond_node, arr_n, idx_n]
                                                                  depth
                                                                  [] [idx_e])
                              (e_locs,e_typs)      <- getTypLocs elem_t
                              -- build the array pointer slicer gate. it will get the new
                              -- array pointer value in the first cARRAY_BLEN bytes of the
                              -- ReadDynArray output.
                              arrptr_n             <- nextInt
                              let arr_ptr_ctx      = mkCtx $
                                                     Gate arrptr_n
                                                          arr_t
                                                          (Slicer $ FieldLoc {
                                                                              Im.byteloc = (0, cARRAY_BLEN),
                                                                              Im.valloc  = (0, 1)
                                                                           })
                                                          [readarr_n]
                                                          depth
                                                          [] []
                              -- add cARRAY_BLEN to all the byte offsets, as the array pointer
                              -- will be output first by the ReadDynArray gate
                              let e_blocs'           = map (projFst (+ cARRAY_BLEN)) $ map Im.byteloc e_locs
                              -- slicer gates
                              is <- replicateM (length e_blocs') nextInt
                              let slicer_ctxs = map mkCtx [Gate i
                                                                t
                                                                (Slicer $ FieldLoc {
                                                                                    Im.byteloc = (boff,blen),
                                                                                    Im.valloc = (off,1)
                                                                                   })
                                                                [readarr_n]
                                                                depth
                                                                [] [] | i           <- is
                                                                      | (boff,blen) <- e_blocs'
                                                                      | off         <- [1..]
                                                                      | t           <- e_typs]

                              -- update the gate location of the array pointer
                              (rv,rv_t,off) <- getRootvarOffset getStrTLocs arr_e
                              spliceVarLocs (off,1) (rv,rv_t) [arrptr_n]

                                  -- we'll add the ReadDynArray and the slicer for the
                                  -- array pointer into the cct here, and not return it
                                  -- with the other slicer contexts, as others should only
                                  -- look for the return vars at the slicers
                              let c_out  = foldl (flip (&)) c2 [readarr_ctx, arr_ptr_ctx]
                              return (c_out, Left slicer_ctxs)

      (EStatic e)       -> genExp' c e

      (EArrayInit name elem_size len) ->
                           do i         <- nextInt
                              let ctx = (mkCtx $ Gate   i
                                                        -- the type field here is a dummy
                                                        (Im.IntT $ Im.lint 12345678)
                                                        (InitDynArray (fromIntegral elem_size)
                                                                      len)
                                                        []
                                                        depth
                                                        [] [])
                                        `trace`
                                        ("Adding InitDynArray of len " << len)
                              return (c, Left [ctx])

      e                 -> error $ "CircGen: unknown expression " ++ (show e)



-- make a graph Context for this gate
mkCtx :: Gate -> CircuitCtx
mkCtx gate =    (map uAdj $ gate_inputs gate,
                 gate_num gate,
                 gate,
                 []) -- no outgoing edges


-- | get all the scalar components within a Struct value of the given type.
getStructLeaves :: Typ -> Exp -> [(Typ,Exp)]
getStructLeaves typ e =
    let leaves = TreeLib.leaves $ TreeLib.iterate expandLevel (typ,e)
    in  leaves
        `logDump`
        ("getStructLeaves (" << typ << ", " << e << ") -> " << leaves)

    where expandLevel (StructT (fields, _), e) =
              let member_es = map (EStruct e) [0..length fields - 1]
                  member_ts = map snd fields
              in  zip member_ts member_es
          expandLevel _                        = []




-- return a type's list of contained types and their locations
getTypLocs :: (TypeTableMonad m) => Typ -> m ([FieldLoc], [Typ])
getTypLocs t =
    do t_full <- Im.expandType [DoTypeDefs] t
       return $ case t_full of
                  (StructT (fields,locs))  ->
                      let types          = map snd fields
                      in  (locs, types)
                  t                        ->
                      ([Im.FieldLoc { Im.valloc =(0, 1),
                                      Im.byteloc=(0, tblen t) }], [t])



-- make an unlabelled edge
uAdj n = ((), n)



-- extract the params of main() from a Prog
extractInputs (Prog pname (ProgTables {funcs=fs})) =
    let (Func _ _ t form_args stms) = fromJustMsg "CircGen::extractInputs" $
                                      Map.lookup Im.cMAINNAME fs
    in  form_args



--------------------
-- state and state access functions
--------------------

-- the state in these computations:
data MyState = MyState { loctable   :: ([LocTable], -- ^ stack of tables for scalars.
                                        ArrLocTable -- ^ A single table for arrays.
                                       ),
                         cond_nodes :: [Gr.Node], -- ^ the stack of the condition nodes in
                                                  -- the current stack of conditionals.
                         counter    :: Int,        -- ^ a counter to number the gates
                                                   -- sequentially
                         typetable  :: TypeTable,  -- ^ the type table is read-only, so
                                                   -- could be in a Reader, but easier to
                                                   -- stick it in here
                         flags      :: [RunFlag]   -- ^ The run-time configuration flags.
                       }


getsLocs        f   = St.gets $ f . loctable
getsTypeTable   f   = St.gets $ f . typetable

getFlags = St.gets flags

getInt              = St.gets counter



-- modify the various parts of the state with some function
modifyLocs f = St.modify $ \st@(MyState{ loctable = x }) -> st { loctable = f x }
modifyInt  f = St.modify $ \st@(MyState{ counter  = x }) -> st { counter = f x }
modifyCondNodes f   = St.modify $ \st@(MyState{ cond_nodes  = x }) -> st { cond_nodes = f x }


-- OutMonad does carry a TypeTable around
instance TypeTableMonad OutMonad where
    getTypeTable = getsTypeTable id




-- update the locations of 'var' by some function of (Maybe [Node])
-- the new value always goes into the top-most scope
-- we always use this, even if the var is certain not to be present (eg. during static
-- initialization), for greater uniformity.
-- an update function which just sets all the gates, already present or not, is
-- (const new_gates)
-- updateScalar :: (Maybe [Gr.Node] -> [Gr.Node]) -> Var -> OutMonad ()
updateScalar f var maps = let curr    = maybeLookup var maps
                              new     = f curr
                              maps'   = modtop (Map.insert var new) maps
                          in  maps'



-- common usages:
-- | Set all the locations for this var
setVarLocs              = setVarLocsFull (map (const True))
-- | Set a sublist of the locations
spliceVarLocs splice (var,t) ls  = setVarLocsFull (mapSplice (const True) (const False)
                                                   (splice
                                                    `logDebug`
                                                    ("spliceVarLocs (" << var << "): splice=" << splice
                                                     << "; ns=" << ls)
                                                   )
                                                  )
                                                  (var,t)
                                                  ls
                                                   
-- | Set just the scalar locs, leave the arrays alone.
setScalarLocs           = setVarLocsFull
                          (map (\a -> case a of
                                        (Just (Array _)) -> False
                                        Nothing          -> True))


-- update or set the locations of a variable, with the given type. deals with sending
-- array components of the variable to the ArrLocTable.

-- the hook function specifies which entries should and should not be inserted, ie. should
-- remain as before. If any fewer than all the locations are provided, obviously an entry
-- for this var must exist already.

-- if inly updating a subset of the locations, the caller will only provide those new
-- locations; we can deal with that here.
setVarLocsFull       :: ([Maybe LocAnnot] -> [Bool])
                     -> (Var,Typ)
                     -> [Gr.Node]
                     -> OutMonad ()
setVarLocsFull hook (var,typ) new_locs =
    do t_full       <- Im.expandType [Im.DoTypeDefs, Im.DoFields] typ
       mb_locs      <- lookupVarLocs var
       let -- prepare for debug messages
           funcid       = "setVarLocsFull (" << var << ", " << typ << ")"
           all_leaves   = getStructLeaves t_full (EVar var)
           -- the old locs if present, or a list of undefines. If a list of undefs, they
           -- should all be replaced in the spliceInIf call below
           locs         = fromMaybe (repeat (-1 :: Gr.Node))
                                    mb_locs
           -- make a preliminary version of the entries, using the old locs if they were
           -- present. The annotations generated here are the final ones.
           locmap_entry = zipWith mk_entry all_leaves locs
           -- which entries do we set now, and which do we keep?
           entries_keep = (hook $ map snd locmap_entry)
           new_entry    = if ( (isNothing mb_locs &&
                                not (and entries_keep)
                               )
                               `logDump`
                                (funcid
                                 << "; new_locs = " << new_locs
                                 << "; entries_keep = " << entries_keep
                                 << "; locmap_entry = " << locmap_entry
                                 << "; all_leaves = " << all_leaves
                                )
                             )
                          ||
                          ( not (or entries_keep) )
                          then error (funcid << " is not setting all locs for " << var
                                      << ", but it does not have an entry already;\
                                          \ or no entries kept to set")
                          else -- put in the new locs where specified
                              spliceInIf snd
                                         -- put in the new location
                                         (\((_, a), True) loc_new  -> (loc_new, a))
                                         -- keep the old location, which was put in
                                         -- locmap_entry already
                                         (\((loc, a), False)       -> (loc, a))
                                         new_locs
                                         (zip locmap_entry entries_keep)
           -- entries for the array loc table
           arr_entries  = [(e,n) | (n, Just (Array e)) <- new_entry]
           -- and now update the actual tables in our state.
       setVarEntry (var,typ) new_entry arr_entries
          `logDump`
          (funcid << " has arr_entries = " << arr_entries)
    where mk_entry (Im.ArrayT _ _, e) n = (n, Just $ Array e)
          mk_entry _                  n = (n, Nothing)


-- update a variable entry
setVarEntry (var,typ) entries arr_entries =
       modifyLocs   (\(locs,arr_locs) ->
                             ((updateScalar (const (entries,typ))
                                            var
                                            locs
                               `logDebug`
                               (let old_entry = case maybeLookup var locs of
                                                  Just (locs,_typ)  -> strShow locs
                                                  Nothing           -> "not present"
                                in  
                                  "setVarEntry (" << var << "): entries=" << entries
                                  << "; arr_entries=" << arr_entries << ";old entry=" << old_entry)
                              )
                             ,
                              (-- insert all (>= 0) new entries into ArrLocTable
                               insertMany arr_entries arr_locs
                              )
                             )
                         )
    

lookupVarLocs :: Var -> OutMonad (Maybe
                                  [Gr.Node]
                                 )
lookupVarLocs   = extractVarLocs (const True)

-- drop entries with an Array annotation.
lookupScalarLocs = extractVarLocs $ \a -> case a of Nothing        -> True
                                                    Just (Array _) -> False

lookupVarTyp var = getsLocs $
                   snd .
                   fromJust .
                   maybeLookup var .
                   fst
                   

-- apply a transformation and filter before returning the nodes.
-- elements where f returns Nothing are filtered out
extractVarLocs :: (Maybe LocAnnot -> Bool)
               -> Var
               -> OutMonad (Maybe
                            [Gr.Node]
                           )
extractVarLocs p var =
    do mb_loc_infos     <- getsLocs $
                           maybeLookup var .
                           fst
       case mb_loc_infos of
         Nothing        -> return Nothing
         Just (loc_infos,_typ)
                        -> do let kepts = [ l | l <- loc_infos,
                                            p $ snd l
                                          ]
                              kepts'    <- mapM patch_info kepts
                              return $ Just kepts'
    where patch_info (n, Nothing)           = return n
          -- lookup in the array table
          patch_info (_, Just (Array exp))  = getsLocs $
                                              fromJustMsg ("getVarLocs array lookup for "
                                                           << exp) .
                                              Mapping.lookup exp .
                                              snd
                           

-- the current depth inside nested conditionals, 0-based
getDepth = do len <- getsLocs $ length . fst -- the number of var tables (one per scope)
              return $ case len of 0 -> 0
                                   _ -> len-1

--------------------
-- state utility functions
--------------------
nextInt :: OutMonad Int
nextInt = do modifyInt (+1)
             getInt

pushScope = modifyLocs $ projFst $ push Map.empty

-- pop the scope stack, and return the top scope/LocTable
popScope  = do scope <- getsLocs $ peek . fst
               modifyLocs $ projFst pop
               return scope

-- | Add a conditional node on the stack
pushCondNode n = modifyCondNodes $ push n

-- | And pop a node on exit from an SIfElse
popCondNode = modifyCondNodes pop

-- | Returns the current conditional node, from the top of the stack.
getCondNode = St.gets (peek . cond_nodes)





instance StreamShow LocAnnot where strShows = showsPrec 0
instance StreamShow (Maybe LocAnnot) where strShows = showsPrec 0

-- ---------------
-- output stuff
-- ---------------


-- StreamShow instances

instance StreamShow Gate where
    strShows = cctShowsGate " :: " " ** "

instance StreamShow Op where
    strShows = cctShowsOp


--
-- a look-alike of the Show class to serialize the circuit for the C++ runtime, so we can
-- use the builtin Show and Read to serialize circuits within Haskell
--
class CctShow a where
    cctShows :: a -> ShowS
    cctShow  :: a -> String

    -- and the same mutually recursive definitions: one or both must be provided
    cctShow x  = (cctShows x) ""
    cctShows x = ((cctShow x) ++)



-- line-oriented format:
--
-- gate number
-- flags, or "noflags"
-- gate (output) type
-- gate operation
-- sources, or "nosrc"
-- comment (the name of the output wire usually)
-- <blank line>
cctShowsGate sep delim
              g@(Gate i typ op srcs depth flags docs)
                   = (delim ++)                                          .
                     -- 1: gate number
                     rec' i                                 .   (sep ++) .
                     -- 2: gate flags
                     rec flags                              .   (sep ++) .
                     -- 3: gate result type
                     (rec typ)                              .   (sep ++) .
                     -- 4: gate operation
                     rec op                                 .   (sep ++) .
                     -- 5: source gates
                     (if null srcs
                      then ("nosrc" ++)
                      else (foldr1 (\f1 f2 -> f1 . (" " ++) . f2)
                                   (map rec' srcs)))        .   (sep ++) .
                     -- 6: gate depth
                     showsPrec 0 depth                      .   (sep ++) .
                     -- 7: comment
                     (if null docs
                      then ("nocomm" ++)
                      -- using the last annotation for all gates except Input, where the
                      -- first one should be the input variable.
                      else (let doc = case op   of  Input           -> last docs
                                                    ReadDynArray    -> last docs
                                                    WriteDynArray _ -> last docs
                                                    _               -> head docs
                            in  ((strShow $ stripVarExp doc) ++) .
                                (case doc of    EStatic e   -> (("static " << strShow e) ++)
                                                _           -> id)
                           )
                     )                                                  .
--                      else ((strShow $ map strip doc) ++))               .
                     (delim ++)

        where rec x         = cctShows x -- recurse
              rec'          = showsPrec 0 -- and go into the Show class



-- get rid of variable annotations in an expression
stripVarExp             = mapExp f
    where f (EVar v)    = (EVar (strip_var v))
          f e           = e


-- need a different rendition of Typ's for the runtime
instance CctShow Typ where
    cctShow = PP.render . Im.docTypMachine



instance CctShow Gate where
    cctShows = cctShowsGate "\n" "\n"


instance CctShow GateFlags where
    cctShows f = case f of Output   -> ("Output" ++)
                           Terminal -> ("Terminal" ++)



instance CctShow [GateFlags] where
    cctShow []     = "noflags"
    cctShow flags  = concat $ intersperse " " $ map cctShow $ flags


-- NOTE: strShows is from the StreamShow class, for which we have many instance
-- definitions in Intermediate.hs
cctShowsOp o =
    case o of
            (Bin op)        -> str "BinOp "    . strShows op
            (Un  op)        -> str "UnOp "     . strShows op
            Input           -> str "Input"
            Select          -> str "Select"
            -- convert a LBool to the matching LInt before displaying.
            (Lit l)         -> str "Lit " . case l of
                                              Im.LInt i
                                                  -> showsPrec 0 i
                                              Im.LBool b
                                                  -> showsPrec 0 $ fromEnum b
            (InitDynArray elemsize
                          len)  -> str "InitDynArray " . rec' elemsize . sp .
                                                         rec' len
            ReadDynArray    -> str "ReadDynArray"

            WriteDynArray
              Im.FieldLoc { Im.byteloc = (off,len) } 
                            -> str "WriteDynArray " . rec' off . sp . rec' len
            Slicer
              (Im.FieldLoc { Im.byteloc = (off,len) })
                            -> str "Slicer " . rec' off . sp . rec' len

            (Print prompt)  -> str "Print " . str prompt
          where rec y   = cctShows y
                rec' y  = showsPrec 0 y
                str     = (++)
                sp      = (" " ++)


instance CctShow Op where
    cctShows = cctShowsOp





showCctGraph :: (Gr.DynGraph gr) => gr Gate b -> String
showCctGraph g =
            let inNodes     = map Gr.node' $ GrBas.gsel isInCtx g
                terms       = UDraw.makeTerm (const "node")
                                             (\gate -> ( ("OBJECT", myShow gate):
                                                         getAttribs gate ) )
                                             inNodes
                                             g
                              `trace`
                              ("Calling UDraw.makeTerm with inNodes=" ++ show inNodes)
            in
              (PP.render $ Lib.doc terms)
                `trace` ("UDraw.makeTerm done")
    where isInCtx (ins,_,_,_) = null ins
          myShow g      = show (gate_num g)
                          -- ++ " @ " ++ show (gate_depth g)
                          ++ "\\n" ++ cctShow (gate_op g)
                             -- show the variable name of input gates, eg:
                             -- "Input -> var_name"
                          ++ concat [(case gate_op g of
                                        Input   -> " -> " ++ strShow (stripVarExp $ last $
                                                                      gate_doc g)
                                        _       -> ""),
                                     (if elem Output $ gate_flags g
                                      then "\\nOutput"
                                      else "")
                                     ]

          getAttribs g  =   concat [(if elem Output $ gate_flags g
                                     then [("COLOR", "light blue")
                                          --, ("_GO",   "rhombus")
                                          ]
                                     else []
                                    ),
                                    (case gate_op g of
                                       Input   -> [("COLOR", "green")
                                                  --, ("_GO",   "rhombus")
                                                  ]
                                       Select  -> [("_GO",   "rhombus")
                                                  ]
                                       _       -> [] -- normal gates
                                    )
                                   ]



-------------------------------------
-- some tests
-------------------------------------

testNextInt = let startState    = MyState { loctable    = ([Mapping.empty], Mapping.empty),
                                            cond_nodes  = [],
                                            counter     = 0,
                                            typetable   = Mapping.empty,
                                            flags       = []
                                          }
                  (out,st)      = St.runState test_f startState
              in  (out,st)
    where test_f = do is <- replicateM 5 nextInt
                      return (is)
