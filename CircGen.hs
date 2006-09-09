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
                testNextInt
               ) where


import Array (array, (!))
import Monad (foldM, mplus, liftM)
import Maybe (fromJust, isJust, fromMaybe)
import List
import Control.Monad.Trans (lift)


import Data.Graph.Inductive.Graph               ((&))
import qualified Data.Graph.Inductive.Graph     as Gr
import qualified Data.Graph.Inductive.Basic     as GrBas
import qualified Data.Graph.Inductive.Graphviz  as Graphviz
import qualified Data.Graph.Inductive.Tree      as TreeGr
import qualified Data.Graph.Inductive.Query.DFS as GrDFS
import qualified Data.Graph.Inductive.Query.BFS as GrBFS

import qualified Text.PrettyPrint               as PP

import qualified Data.Map                       as Map
import qualified Data.Tree                      as Tree

import qualified Control.Monad.State            as St

import SashoLib
import UDraw
-- import Sasho.XDR

import qualified    Container                   as Cont
import              Mapping

import Common (trace)

import Intermediate as Im hiding (VarTable)



-- should we generate SPrint gates for print() statements?
cDOPRINT = False

-- should we store ELit gate locations, and only generate one gate per literal?
cSTORE_LITS = True



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

  -- read from a dynamic array; inputs: [array, index]; output is
  -- 1: the new array value (or rather pointer, as the runtime will manage the actual
  --    values), and
  -- 2: of the array element type: either a basic type like Int etc, or a list of Slicer
  --    gates which extract the basic components of the complex type in this array
  | ReadDynArray

  -- initialize an array
  -- parameters:
  -- 1) element size in bytes
  -- 2) number of elems
  | InitDynArray Int Integer

  -- update array; inputs = [array, index, val1, val2, ...]; output is
  -- the updated array. The parameters are:
  -- - a slice of where inputs (concatenated) should end up in the array element.
  --
  -- The input will consist of multiple gates in case of a complex
  -- type, and the runtime can just concat the values to get a lump to
  -- write to the (sliced) array location
  | WriteDynArray Im.FieldLoc

  -- an input gate for a primitive type, IntT or BoolT
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


-- needed to lookup a variable's current gate number
-- Structs and arrays occupy multiple gates, so need to actually store
-- a list of gate numbers
type VarTable = Map.Map Var [Int]



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
    let startState      = MyState { vartable    = [Map.empty],
                                    counter     = 0,
                                    typetable   = type_table,
                                    lit_table   = Mapping.empty
                                  }
        (circ, st)      = St.runState (genCircuitM stms args)
                                      startState
        clipped_circ    = clip_circuit circ
                                `trace` ("The full circuit: " << show circ)
        renum_circ      = renumber clipped_circ
                                `trace` ("The clipped circuit " << show clipped_circ)
        -- expand all Typ fields in the gates, as it helps with printing out the types
        -- (using docTypMachine)
        flat_circ       = (map (expandGateTyp type_table) $ flatten_cicrcuit renum_circ)
                                `trace` ("The renumbered circuit " << show renum_circ)
    in (renum_circ, flat_circ)
         `trace` ("The DFS forest: " << genDFSForest renum_circ)


getUFoldOrder g = Gr.ufold f "" g
    where f (_,n,g,_) s = show n ++ " " ++ s


genDFSForest g = Tree.drawForest $ GrDFS.dffWith' strNode g
    where strNode (_,n,_,_) = show n



-- keep only gates which are reverse-reachable from terminal gates
clip_circuit :: Circuit -> Circuit
clip_circuit c = let c_rev          = GrBas.grev c
                     out_nodes      = map Gr.node' $
                                      GrBas.gsel isTerminal c_rev
                     reach_nodes    = GrBFS.bfsn out_nodes c_rev
                     reach_gr       = keepNodes reach_nodes c
                 in  reach_gr
    where isTerminal = elem Terminal . gate_flags . Gr.lab'



-- renumber the nodes so they are consecutive, in the same order as the original numbering
renumber :: Circuit -> Circuit
renumber g   =  Gr.gmap doRenum g
          -- the returned map will map from current numbering to consecutive numbering
    where renumMap                      = let ins   = map Gr.node' $ GrBas.gsel isRootCtx g
                                              nodes = sort $ GrBFS.bfsn ins g
                                          in  array (minimum nodes, maximum nodes)
                                                    (zip nodes [0..])
                                                    `trace` ("renumber nodes: " << nodes)
          doRenum (ins,node,gate,outs)  = ( map (projSnd renum) ins,
                                            renum node,
                                            gateProjNum renum $
                                               gateProjSrcs (map renum) $ gate,
                                            map (projSnd renum) outs )
          renum node                    = renumMap ! node
          isRootCtx (ins,_,_,_)         = null ins



flatten_cicrcuit :: Circuit -> [Gate]
flatten_cicrcuit c = let gates  = GrDFS.topsort' c
                         gates' = ins_to_front gates
                     in  gates'
          -- get the input gates to the front of the list. this partition should not
          -- disturb the topological sort
    where ins_to_front gates =
              let (ins, others) = List.partition ((== Input) . gate_op)
                                                 gates
              in  ins ++ others

expandGateTyp :: TypeTable -> Gate -> Gate
expandGateTyp typetable g@(Gate { gate_typ = t })    =
    let t' = Im.expandType' typetable [Im.DoAll] t
    in
      g { gate_typ = t'
                     `trace` ("expandGateTyp " << t << " -> " << t')
        }

    
-- | remove the nodes (and associate edges) not in 'keeps' from 'g'.
keepNodes :: Gr.Graph gr => [Gr.Node] -> gr a b -> gr a b
keepNodes keeps g = Gr.delNodes (Gr.nodes g \\ keeps) g



-- and the stateful computation
genCircuitM :: [Stm] -> [TypedName] -> OutMonad Circuit
genCircuitM stms args =
    do input_gates <- genInputs args
       foldM genStm input_gates stms


genInputs :: [TypedName] -> OutMonad Circuit
genInputs names     = do gatess     <- mapM (createInputGates True) names
                         return     (Gr.mkGraph (map gate2lnode (concat gatess)) [])



-- create the input gates for this argument to main(), and add the vars
-- to the top-level VarTable
-- will make this return a [Gate], for when we deal with arrays etc
--
-- we'll only insert mappings into the VarTable at the top level, not
-- in recursive calls (then we'd be inserting struct field names as
-- actual vars)

createInputGates :: Bool -> TypedName -> OutMonad [Gate]
createInputGates addvars (name, typ) =
    do let var         = add_vflags [FormalParam] (VSimple name)
       type_table      <- getTypeTable
       case typ of
        (StructT (fields,_))
                -> do gates <- concatMapM (createInputGates False) fields
                      let is = map gate_num gates
                      if addvars then setVarAddrs is var else return ()
                      return gates


        (SimpleT tname)
                -> let typ' = fromJust $ Map.lookup tname type_table
                   in  createInputGates True (name, typ')


        -- IntT, BoolT, ArrayT (for dynamic arrays):
        _         -> do i <- nextInt
                        if addvars
                          then setVarAddrs [i] var
                                   `trace`
                                   ("inserting input var " << var << " into VarTable")
                          else return ()
                        -- FIXME: this adds an incorrect doc
                        -- annotation to the gate - it show struct
                        -- field names as variable names
                        gate <- mkGate i typ (EVar var)
                        return [gate]

    where mkGate i typ doc_exp = do typ_full <- Im.expandType [DoAll] typ
                                    return $ Gate i
                                                  typ_full
                                                  Input
                                                  []
                                                  0
                                                  []
                                                  [doc_exp]





-- this just adds an obvious dummy entry of the correct size in the var table
-- for this lval.
-- we only need to do it for structs which are variables, and not
-- expressions (ie. parts of other complex types)
genComplexInit lval size =
    case lval of
      (EVar var)        -> setVarAddrs (replicate size (-12345678)) var
      _                 -> return ()




-- get the gates corresponding to expression 'e', if necessary adding
-- new gates to the circuit, and add e_doc as the last annotation of
-- that gate
-- returns the new circuit, and the gates where 'e' is
-- also have the gates pass through a hook which may update them, eg. add
-- flags
genExpWithDoc :: Circuit ->
                 Maybe (Gate -> Gate) ->
                 Exp ->
                 Exp ->
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
                    m (Var, Int)
getRootvarOffset loc_extr exp =
    do let rec = getRootvarOffset loc_extr
       case exp of
        (EVar v)            -> return (v, 0)
        (EStruct str_e idx) -> do  (v, o)     <- rec str_e
                                   fld_info   <- getStrTParams str_e
                                   let locs   = loc_extr fld_info
                                       preceding  = sum $
                                                    map snd $
                                                    take idx locs
                                   return (v, o + preceding)
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
    case stm of

      -- do away with lval type annotations for now
      (SAss (ExpT _ lval) val) -> genStm circ (SAss lval val)

      -- this is a message from the typechecker that a Struct or such
      -- will be coming along just now.
      (SAss lval (ExpT _ (EStructInit size))) ->
          do genComplexInit lval size
             return circ

      s@(SAss lval@(EVar var) exp) ->
          do -- var_table  <- getVars
             circ'      <- checkOutputVars circ var Nothing
                               {- `trace` ("genStm " << s <<
                                        "; var=" << var <<
                                        "; vartable=" << var_table) -}
             (c',nodes) <- genExpWithDoc circ'
                                          (addOutputFlag var)
                                          exp
                                          lval
             setVarAddrs nodes var
             return c'

      -- get the gates for this 'val', and set the address of 'lval'
      -- to those gates

      s@(SAss lval@(EStruct str_e idx) val) ->
         {-# SCC "stm-ass-estruct" #-}
          do type_table    <- getTypeTable
             (_,locs)      <- getStrTParams str_e
             let (off,len)  = valloc $ locs !! idx
             (rv,lval_off)  <- getRootvarOffset getStrTLocs lval
             c2             <- checkOutputVars circ rv (Just (lval_off,len))
             (c3, gates)    <- genExpWithDoc c2
                                             (addOutputFlag rv)
                                             val
                                             lval
             arr_info       <- extractEArr lval
             case arr_info of
               Nothing              -> -- just update the lval location(s)
                                       do spliceVar (lval_off,len) gates rv
                                          return c3
               Just ((EArr arr_e idx_e), arr_off, locs)
                                    -> -- need to generate a WriteDynArray gate!
                                       -- eg: for x.y[i].z, we will:
                                       -- - add a WriteDynArray gate for x.y, limited to .z
                                       -- - update the gate location for x.y
                                       {-# SCC "have_array_in_expression" #-}
                                       do (c4, [arr_n]) <- genExp c3 arr_e
                                          (c5, [idx_n]) <- genExp c4 idx_e
                                          depth         <- getDepth
                                          i             <- nextInt
                                          let prep_slice locs   = (fst $ head locs, sum $ map snd locs)
                                              -- get the slice parameters from the field
                                              -- location info given by extractEArr above
                                              slice             = Im.FieldLoc
                                                                  { byteloc = prep_slice $ map Im.byteloc locs,
                                                                    valloc  = prep_slice $ map Im.valloc  locs }
                                              (ExpT arr_t _)    = arr_e
                                              ctx        = mkCtx $
                                                           Gate i
                                                                arr_t
                                                                (WriteDynArray slice)
                                                                ([arr_n, idx_n] ++ gates)
                                                                depth
                                                                [] []
                                          spliceVar (arr_off,1) [i] rv
                                                        `trace`
                                                        ("WriteDynArray: rv = " << rv
                                                         << "; arr_off=" << arr_off)
                                          return $ ctx & c5


      -- quite similar to the above. really need to structure this better
      s@(SAss lval@(EArr arr_e idx_e) val) ->
          {-# SCC "stm-ass-earr" #-}
          do (rv,off)           <- getRootvarOffset getStrTLocs arr_e
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
             i                  <- nextInt
             let (ExpT arr_t _)  = arr_e
                 ctx             = {-# SCC "mkCtx" #-}
                                   (mkCtx $ Gate i
                                                 arr_t
                                                 -- NOTE: writing -1 here to mean "the end"
                                                 (WriteDynArray $ Im.FieldLoc (0,-1) (0,-1))
                                                 ([arr_n, idx_n] ++ gates)
                                                 depth
                                                 [] [])
             spliceVar (off,1) [i] rv
             return $ {-# SCC "ctx-&-c5" #-} (ctx & c5)
                        `trace`
                        ("SAss to EArr: \"" ++ show stm ++
                         "\n; inserting " ++ show ctx)


      -- NOTE: after HoistStm.hs, all conditional tests are EVar
      (SIfElse test@(EVar testVar)
               (locs1, stms1)
               (locs2, stms2))  ->
          {-# SCC "stm-ass-ifelse" #-}
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

      -- we will wire all the variables in x to go through this gate
      -- a bit of a hassle as there may be one or more gates feeding into here (in case of
      -- a struct). So, add slice gates
      (SPrint prompt xs)      -> if not cDOPRINT
                                 then return circ
                                 else
                                   do i              <- nextInt
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



      s -> error $ "Unknow genStm on " << s

    where addOutputFlag var =
              case strip_var var of
                (VSimple "main") -> Just (\gate -> gate {gate_flags = [Output, Terminal]})
                _                -> Nothing



-- do most of the work for a single expression in an SPrint
doSPrintExp prompt circ e =        do (circ', x_ns)     <- genExp circ e
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
                                                                         -> let blen =
                                                                                    Im.typeLength Im.tblen
                                                                                                  t_full
                                                                            in
                                                                              ([t_full],
                                                                               [Im.FieldLoc (0,blen)
                                                                                            (0,1)])

                                      setVarAddrs slice_is var

                                      return (circ', x_ns, ts, slice_is, locs)



-- extract an array sub-expression from the given Exp
-- also return the list of field-locations (in bytes) of the array element type
-- which are covered by the whole expression
-- and also return the offset of the array expression under its root variable.
-- by example, say we have an expression x.y[i].z
-- then, the outputs are
-- ( the array expression x.y[i],
--   the offset of .y under x,
--   the SliceAddr's of .z under x.y[i]
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
    do  (locs,_)   <- lift $ getTypLocs elem_t

        -- here we need the offset in words (int/bool)
        (rv,off)    <- lift $ getRootvarOffset getStrTLocs arr_e
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
                            `trace`
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
-- optionally an offset and length to limit the flag removal to some
-- of the gates, in case only part of a complex output var is being
-- modified.
checkOutputVars :: Circuit -> Var -> Maybe (Int,Int) -> OutMonad Circuit
checkOutputVars c var mb_gate_loc
    | strip_var var == VSimple "main" =
        -- remove the output flags there
        do mb_vgates    <- getsVars $ maybeLookup var
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
                        c' = foldl (updateLabel (\g -> g{gate_flags = []}))
                                   c
                                   (filter (/= -12345678) vgates')
                    return c'
    | otherwise =
        return c `trace` ("checkOutputVars non-matching var: " << var)


-- update the label for a given node in a graph, if this node is present
-- NOTE: strict evaluation
updateLabel :: (Gr.DynGraph gr) => (a -> a) -> gr a b -> Gr.Node -> gr a b
updateLabel f gr node = let (mctx,gr')      = {-# SCC "#extract" #-}
                                              strictEval $
                                                       Gr.match ({-# SCC "#node" #-} node)
                                                                ({-# SCC "#gr"   #-} gr)
                            ctx             = fromJustMsg ("updateLabel " << node)
                                                          mctx
                        in {-# SCC "re-insert" #-} (tup4_proj_3 (strictEval f) $! ctx) & (strictEval gr')
{-                          
                        in  case id $! mctx of
                              Nothing   -> {-# SCC "ret-id" #-}     gr
                              Just ctx  -> {-# SCC "ret-new-gr" #-} (tup4_proj_3 f ctx) &
                                                                    (strictEval gr')
-}




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
        sources = (map varSources vars) `trace` ("non-local scope vars: " << vars)

    in  foldM addSelect circ $ zip vars sources

    where -- a var is non-local if was not declared in this scope,
          -- *and* it appears in the parent scope (needed in the case of
          -- generated vars)
          -- FIXME: do generated vars ever have to be selected? seems not! they are
          -- intrinsically very local in their usage, and so do not need to persist across
          -- scopes
          nonLocal var = (not $ any (Cont.member (stripScope var))
                                    [ifLocs, elseLocs])
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

                           in  out `trace` ("varSources for " << var << ": "
                                            << out)

          -- find the gates for a var, looking in the given scope stack
          gates var scopes = fromJust $ maybeLookup var scopes

          -- add Select gates for a free variable, and update its
          -- wire locations, to the new Select gates
          -- need multiple select gates if it's a struct, but in this
          -- case we add Select only for the gates which were actually
          -- updated in this scope
          -- this function is quite nasty!
          addSelect c (var, in_gates@(gates_true', gates_false'))
              = do let changed_gates    = filter (\(x,y) -> x /= y) $ uncurry zip in_gates
                   ts           <- mapM (getSelType c) changed_gates
                   -- get the right number of new int's
                   is           <- replicateM (length ts) nextInt
                   let ctxs      = zipWith3 mkCtx' is ts changed_gates
                       new_gates = foo (uncurry zip in_gates) is
                   setVarAddrs new_gates var
                   -- work all the new Contexts into circuit c
                   return (foldl (flip (&)) c ctxs)

          mkCtx' i t (true_gate,false_gate) =
              let src_gates = [testGate, true_gate, false_gate]
                  depth     = (length parentScope) - 1
              in  mkCtx (Gate i t Select src_gates depth [] [])


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



-- add a list of contexts (ie. gates) to a circuit, return the new circuit
addCtxs circ ctxs = foldl (flip (&)) circ ctxs



-- figure out the type of a Select gate, based on the type of its two
-- inputs
getSelType gr (node1, node2)
    = do let gates = map (fromJust . Gr.lab gr) [node1, node2]
         -- expanding types into a canonical form
         types <- mapM (Im.expandType [DoTypeDefs]) $ map gate_typ gates
         case types `trace` ("getSelType of gates " << gates) of
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
    case exp `trace` ("genExp' " << exp) of
      (BinOp op e1 e2)  -> do i <- nextInt
                              (c1, gates1) <- genExp c   e1
                              let gate1 = case gates1 of
                                            [g1]   -> g1
                                            _      -> error ("BinOp " << exp <<
                                                             " arg1 got multiple gates: " <<
                                                             gates1)
                              (c2, gates2) <- genExp c1  e2
                              let gate2 = case gates2 of
                                            [g]    -> g
                                            _      -> error ("BinOp " << exp <<
                                                             " arg2 got multiple gates: " <<
                                                             gates2)
                              let ctx       = mkCtx $ Gate i VoidT (Bin op) [gate1, gate2] depth [] []
                              return (c2, Left [ctx])

      (UnOp op e1)      -> do i <- nextInt
                              (c1, [gate1]) <- genExp c   e1
                              let ctx       = mkCtx $ Gate i VoidT (Un op) [gate1] depth [] []
                              return (c1, Left [ctx])

      (EVar var)        -> do var_table  <- getVars
                              gates <- (getsVars $
                                        fromJustMsg ("CircGen: Lookup Var " << var) .
                                        maybeLookup var)

                              return (c, Right gates)
                                         `trace`
                                         ("genExp' EVar " << var << ", vartable=" << show var_table <<
                                          " -> " << gates)


      (ELit l)        -> {-# SCC "Exp-ELit" #-}
                         if cSTORE_LITS
                         then
                           do mb_lit_addr   <- getLit l
                              case mb_lit_addr of
                                Just addr   -> return (c, Right [addr])
                                Nothing     -> do i     <- nextInt
                                                  addLit l i
                                                  let ctx   = mkCtx (Gate i
                                                                          (IntT 32)
                                                                          (Lit l)
                                                                          []
                                                                          depth
                                                                          [] [])
                                                  return (c, Left [ctx])
                         else   -- not cSTORE_LITS
                           do i         <- nextInt
                              let ctx   = mkCtx (Gate i (IntT 32) (Lit l) [] depth [] [])
                              return (c, Left [ctx])                           



{-
      (ELit l)          -> do i         <- nextInt
                              let ctx   = mkCtx (Gate i (IntT 32) (Lit l) [] depth [] [])
                              return (c, Left [ctx])
-}



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
                                                   `trace`
                                                   ("genExp' EStruct " << exp
                                                    << " struct gates = " << gates)
                              let (off,len)      = (Im.valloc $ locs !! idx)
                                                   `trace`
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
                                  -- get the array type and the element type, from the
                                  -- array expression annotations
                              let (ExpT arr_t@(ArrayT elem_t _) _)   = arr_e
                                  readarr_ctx       = mkCtx (Gate readarr_n
                                                                  VoidT
                                                                  ReadDynArray
                                                                  [arr_n, idx_n]
                                                                  depth
                                                                  [] [])
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
                              (rv,off)  <- getRootvarOffset getStrTLocs arr_e
                              spliceVar (off,1) [arrptr_n] rv

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

      e                 -> error $ "CircGen: unknown expression " << e



-- make a graph Context for this operator, node number and source
-- gates
mkCtx :: Gate -> CircuitCtx
mkCtx g@(Gate node _ _ srcs _ _ _) = let ctx = (map uAdj srcs,
                                                node,
                                                g,
                                                []) -- no outgoing edges
                                    in ctx





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
    let (Func _ _ t form_args stms) = fromJust $ Map.lookup "main" fs
    in  form_args



--------------------
-- state and state access functions
--------------------

-- the state in these computations:
data MyState = MyState { vartable   :: [VarTable], -- stack of table Var -> [Node]
                         counter    :: Int,        -- a counter to number the gates
                                                   -- sequentially
                         typetable  :: TypeTable,  -- the type table is read-only, so
                                                   -- could be in a Reader, but easier to
                                                   -- stick it in here
                         lit_table  :: [(Im.Lit,Gr.Node)]      -- a Mapping of literals (Integer
                                                               -- or Bool)
                                                               -- to Node number, so we can
                                                               -- stick to one gate per
                                                               -- literal.
                                                               -- instantiated as a list
                                                               -- for now
                       }
{-
instance St.MonadState Int (St.State MyState) where
    get = getInt
    put = tup3_get2 >>= St.put
-}



getsVars        f   = St.gets $ f . vartable
getsTypeTable   f   = St.gets $ f . typetable

getInt              = St.gets counter
getVars = getsVars id


-- get the gate for a literal, in a Monad
-- fails if the literal does not yet have a gate
getLit l = St.gets $ Mapping.lookup l . lit_table


-- modify the various parts of the state with some function
modifyVars f = St.modify $ \st@(MyState{ vartable = x }) -> st { vartable = f x }
modifyInt  f = St.modify $ \st@(MyState{ counter  = x }) -> st { counter = f x }
modifyLits f = St.modify $ \st@(MyState{ lit_table  = x }) -> st { lit_table = f x }

-- OutMonad does carry a TypeTable around
instance TypeTableMonad OutMonad where
    getTypeTable = getsTypeTable id


-- add a gate number for this literal
addLit l addr = modifyLits $ Mapping.insert l addr




-- update the location of 'var' by some function of (Maybe [Node])
-- the new value always goes into the top-most scope
-- we always use this, even if the var is certain not to be present (eg. during static
-- initialization), for greater uniformity.
-- an update function which just sets all the gates, already present or not, is
-- (const new_gates)
updateVar :: (Maybe [Gr.Node] -> [Gr.Node]) -> Var -> OutMonad ()
updateVar f var = modifyVars $ \maps -> let curr    = maybeLookup var maps
                                            new     = f curr
                                            maps'   = modtop (Map.insert var new) maps
                                        in  maps'

-- two common usages:
-- just set the var gates completely, regardless if the var is already present or not
setVarAddrs new_gates = updateVar (const new_gates)
-- splice in new gates into a part of the current list. error if the var not already
-- present
spliceVar loc@(off,len) new_gates var = updateVar (splice loc new_gates .
                                                   fromJustMsg ("spliceVar " << var))
                                                  var
                                          `trace`
                                          ("spliceVar " << var << " at " << off << "," << len
                                           << " with " << new_gates)

-- the current depth inside nested conditionals, 0-based
getDepth = do len <- getsVars length -- the number of var tables (one per scope)
              return (if len > 0
                      then len - 1
                      else 0)

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




-- ---------------
-- output stuff
-- ---------------


-- StreamShow instances

instance StreamShow Gate where
    strShows = cctShowsGate " :: " " ** "


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
              g@(Gate i typ op srcs depth flags doc)
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
                     (if null doc
                      then ("nocomm" ++)
                      -- FIXME: outputting the latest addition to the comment stack here
                      --(the one at the top), not sure if this is best
                      else ((strShow $ strip $ peek doc) ++))             .
--                      else ((strShow $ map strip doc) ++))               .
                     (delim ++)

        where rec x         = cctShows x -- recurse
              rec'          = showsPrec 0 -- and go into the Show class

              -- get rid of variable annotations in an expression
              strip         = mapExp f
              f (EVar v)    = (EVar (strip_var v))
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





-- need to get rid of newlines in the nodes
showCctGraph :: (Gr.DynGraph gr) => gr Gate b -> String
showCctGraph g =
            let inNodes     = map Gr.node' $ GrBas.gsel isInCtx g
{-
                trees       = makeSimpleTree inNodes g
            in
              Tree.drawForest $ map (mapTree show) trees
-}
                terms       = UDraw.makeTerm (const "node")
                                             (\gate -> [("OBJECT", myShow gate)])
                                             inNodes
                                             g
                              `trace`
                              ("Calling UDraw.makeTerm with inNodes=" ++ show inNodes)
            in
              (PP.render $ UDraw.doc terms)
                `trace` ("UDraw.makeTerm done")
    where isInCtx (ins,_,_,_) = null ins
          myShow g = show (gate_num g)
                     ++ " @ " ++ show (gate_depth g)
                     ++ "\\n" ++ cctShow (gate_op g)


{-
instance XDR Gate where
    encodes g = foldr1 (.) $
                map encodes [gate_num g, gate_typ g, gate_op g, gate_inputs g,
                             gate_depth g, gate_flags g, show (gate_doc g)]
    decodes g = undefined
-}

-------------------------------------
-- some tests
-------------------------------------

testNextInt = let startState    = MyState { vartable    = [Map.empty],
                                            counter     = 0,
                                            typetable   = Map.empty,
                                            lit_table   = Mapping.empty
                                          }
                  (out,st)      = St.runState test_f startState
              in  (out,st)
    where test_f = do is <- replicateM 5 nextInt
                      return (is)
{-
{- Generated by DrIFT (Automatic class derivations for Haskell) -}
{-# LINE 1 "CircGen.hs" #-}
{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance XDR GateFlags
where
    encode x = encode (xToInt x)
    where
        -- compute constructor's integer code
        xToInt (Output) = 0
    decode xdr = intToX (decode xdr)
    where
        intToX i = fromJust $ lookup i  [(0,Output)]

instance XDR Op
where
    encode x =
        -- get the XDR union discriminator encoding
        let (cons_enc,args_enc) = my_encode x
        in
        cons_enc ++ args_enc
        where
            -- Compute encoding of the constructor and its arguments
            my_encode (Bin v0) = let cons_enc = encode 0
                                     args_enc = encode v0
                                 in
                                 (cons_enc, args_enc)
            my_encode (Un v0) = let cons_enc = encode 1
                                    args_enc = encode v0
                                in
                                (cons_enc, args_enc)
            my_encode (ReadDynArray) = let cons_enc = encode 2
                                           args_enc =
                                       in
                                       (cons_enc, args_enc)
            my_encode (WriteDynArray v0) = let cons_enc = encode 3
                                               args_enc = encode v0
                                           in
                                           (cons_enc, args_enc)
            my_encode (Input) = let cons_enc = encode 4
                                    args_enc =
                                in
                                (cons_enc, args_enc)
            my_encode (Select) = let cons_enc = encode 5
                                     args_enc =
                                 in
                                 (cons_enc, args_enc)
            my_encode (Slicer v0) = let cons_enc = encode 6
                                        args_enc = encode v0
                                    in
                                    (cons_enc, args_enc)
            my_encode (Lit v0) = let cons_enc = encode 7
                                     args_enc = encode v0
                                 in
                                 (cons_enc, args_enc)
    decode = undefined

instance XDR Gate
where
    encode x =
        -- get the XDR union discriminator encoding
        let (cons_enc,args_enc) = my_encode x
        in
        cons_enc ++ args_enc
        where
            -- Compute encoding of the constructor and its arguments
            my_encode (Gate v0 v1 v2 v3 v4 v5 v6) = let cons_enc = encode 0
                                                        args_enc = encode v0 ++
                                                                   encode v1 ++
                                                                   encode v2 ++
                                                                   encode v3 ++
                                                                   encode v4 ++
                                                                   encode v5 ++
                                                                   encode v6
                                                    in
                                                    (cons_enc, args_enc)
    decode = undefined

--  Imported from other files :-
-}
