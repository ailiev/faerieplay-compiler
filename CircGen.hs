-- code to generate a circuit from an unrolled list of statements,
-- consisiting only of SAss, and SIfElse.

module CircGen (
                genCircuit,
                clip_circuit,
                extractInputs,
                showCct,

                testNextInt
               ) where


import Monad (foldM, mplus, liftM)
import Maybe (fromJust, isJust, fromMaybe)
import List


import Data.Graph.Inductive.Graph               ((&))
import qualified Data.Graph.Inductive.Graph     as Gr
import qualified Data.Graph.Inductive.Basic     as GrBas
import qualified Data.Graph.Inductive.Graphviz  as Graphviz
import qualified Data.Graph.Inductive.Tree      as TreeGr
import qualified Data.Graph.Inductive.Query.DFS as GrDFS
import qualified Data.Graph.Inductive.Query.BFS as GrBFS

import qualified Data.Map                       as Map
import qualified Control.Monad.State            as St

import SashoLib

import qualified Container as Cont
import Common (trace)

import Intermediate as Im hiding (VarTable)



-- gate flags
data GateFlags = Output deriving (Eq,Ord)


-- the gate operations
data Op =
    Bin Im.BinOp
  | Un  Im.UnOp

  -- read from a dynamic array; inputs: [array, index]; output is of
  -- the array element type: either a basic type like Int etc, or a
  -- StructSplit gate to handle a Struct
  -- The [Int] is used to split a structure into its simple elements: it
  -- contains the byte-address of the *last* byte of each element
  | ReadDynArray -- [Int]

  -- update array; inputs = [array, index, val1, val2, ...]; output is
  -- the updated array. The parameter is a slice of where inputs
  -- (concatenated) should end up in the array element.
  --
  -- The input will consist of multiple gates in case of a complex
  -- type, and the runtime can just concat the values to get a lump to
  -- write to the (sliced) array location
  | WriteDynArray (Int,Int)

  -- an input gate for a primitive type, IntT or BoolT
  | Input

  -- select one of two values based on a test; the input wires are to
  -- be: [test, src_true, src_false]
  | Select

  -- split a structure into its elements. The [Int] is the
  -- byte-address of each element
  -- NOTE: has been subsumed into ReadDynArray for now
  --  | StructSplit [Int]

  -- a gate which takes only a part of its input, specified by a
  -- zero-based offset and a length, in bytes;
  -- used after a ReadDynArray, to collect the output of ReadDynArray
  -- into separate gates
  | Slicer (Int,Int)

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

type GateDoc = [Exp]


setFlags newfl (Gate g1 g2 g3 g4 fl    g6) =
               (Gate g1 g2 g3 g4 newfl g6)

setGateType newt        (Gate g1 typ  g3 g4 g5 g6) =
                        (Gate g1 newt g3 g4 g5 g6)


-- NOTE: the gate number is the same as the Node value in the Graph
-- (which is also an Int)
-- helper to make a labelled Node from a Gate
gate2lnode :: Gate -> Gr.LNode Gate
gate2lnode g@(Gate i _ _ _ _ _) = (i,g)

-- accessors for some of the Gate fields
gateNode = fst . gate2lnode
gateType (Gate _ t _ _ _ _) = t
gateFlags (Gate _ _ _ _ fl _) = fl



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
    let startState      = ([Map.empty], 0, type_table)
        (circ, st)      = St.runState (genCircuitM stms args)
                                      startState
        clipped_circ    = clip_circuit circ
        top_circ   = GrDFS.topsort' circ
    in clipped_circ


-- keep only gates which are reverse-reachable from the output gates
clip_circuit :: Circuit -> Circuit
clip_circuit c = let c_rev          = GrBas.grev c
                     out_nodes      = map Gr.node' $
                                      GrBas.gsel isOutCtx c_rev
                     reach_nodes    = GrBFS.bfsn out_nodes c_rev
                     reach_gr       = keepNodes reach_nodes c
                 in  reach_gr
    where isOutCtx = elem Output . gateFlags . Gr.lab'



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
        (StructT (fields,_,_))
                -> do gates <- concatMapM (createInputGates False) fields
                      let is = map gateNode gates
                      if addvars then updateVar (const is) var else return ()
                      return gates

        (SimpleT tname)
                -> let typ' = fromJust $ Map.lookup tname type_table
                   in  createInputGates True (name, typ')

        -- IntT, BoolT, ArrayT (for dynamic arrays):
        _         -> do i <- nextInt
                        if addvars
                          then (updateVar (const [i]) var)
                                   `trace`
                                   ("inserting input var " << var << " into VarTable")
                          else return ()
                        -- FIXME: this adds an incorrect doc
                        -- annotation to the gate - it show struct
                        -- field names as variable names 
                        return [mkGate type_table i typ (EVar var)]

    where mkGate type_table i typ doc_exp = Gate i
                                                 (expandType type_table typ)
                                                 Input
                                                 []
                                                 []
                                                 [doc_exp]





-- this just adds a dummy entry (a large negative) of the correct size in the var table
-- for this lval.
-- we only need to do it for structs/arrays which are variables, and not
-- expressions (ie. parts of other complex types)
genComplexInit lval size =
    case lval of
      (EVar var)        -> updateVar (const $ replicate size (minBound)) var
      _                 -> return ()


-- update a range (offset and length) of a list.
-- more precisely, replace the range (offset,len) with 'news' (regardless of its length)
splice (offset,len) news l = (take offset l) ++
                             news ++
                             (drop (offset + len) l)


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

                            (Right nodes) ->
                                -- no new gate generated, but we will
                                -- update the gate doc and apply the hook
                                let c'' = foldl (updateLabel processGate)
                                                c'
                                                nodes
                                              
                                in  (c'', nodes)
       return (c'', nodes)

    where addGateDoc exp (Gate g1 g2 g3 g4 g5 doc) =
                          Gate g1 g2 g3 g4 g5 new_doc
              where new_doc = push (stripExpT exp) doc
          processGate g     = let g'  = maybeApply ghook g
                                  g'' = (addGateDoc e_doc g')
                              in  g''
          processCtx        = tup4_proj_3 processGate


-- update the label of a graph Context
updateCtxLab new_label = tup4_proj_3 (const new_label)



-- get the offset of an expression, relative to the simple variable
-- under which this expression is, eg. for a struct expression x.y.z we want the
-- offset of field 'y.z' under struct 'x'; also return what is the root
-- variable, 'x' in this example
-- the 'loc_extr' parameter is a function which specifies which struct
-- locations we use, primitive type (tup3_get2) or byte (tup3_get3)

getRootvarOffset :: TypeTable ->
                    ( ([TypedName], [FieldLoc], [FieldLoc]) -> [FieldLoc] ) ->
                    Exp ->
                    (Var, Int)
getRootvarOffset tab loc_extr exp =
    let rec = getRootvarOffset tab loc_extr in
    case exp of
      (EVar v)              -> (v, 0)
      (EStruct str_e idx)   -> let (v, o)     = rec str_e
                                   fld_info   = getStrTParams tab str_e
                                   locs       = loc_extr fld_info
                                   preceding  = sum $
                                                map snd $
                                                take idx locs
                               in  (v, o + preceding)
      (ExpT t e)            -> rec e
      (EArr arr_e idx_e)    -> rec arr_e
      e                     -> error $
                               "CircGen: getRootvarOffset: invalid lval "
                               << e
                                                      

-- get the field parameters of a StructT, from the expression carrying the struct
getStrTParams tab (ExpT t _)   = let (StructT field_params) = expandType tab t
                                 in  field_params
getStrTParams _   e             = error $
                                  "getStrTParams: unexpected expression "
                                  << e


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
             updateVar (const nodes) var
             return c'

      -- get the gates for this 'val', and set the address of 'lval'
      -- to those gates
      s@(SAss lval@(EStruct str_e idx) val) ->
          do type_table    <- getTypeTable
             let (_,locs,_) = getStrTParams type_table str_e
                 (off,len)  = locs !! idx
                               -- (off,len) are in primitive types, not bytes
                 (rv,
                  lval_off) = getRootvarOffset type_table tup3_get2 lval
             c2             <- checkOutputVars circ rv (Just (lval_off,len))
             (c3, gates)    <- genExpWithDoc c2
                                             (addOutputFlag rv)
                                             val
                                             lval
             case extractEArr type_table lval of
               Nothing              -> -- just update the lval location(s)
                                       do updateVar
                                            (applyWithDefault (splice (lval_off,len) gates)
                                                              gates)
                                            rv
                                          return c3
               Just ((EArr arr_e idx_e), arr_off, blocs)
                                    -> -- need to generate a WriteDynArray gate!
                                       -- eg: for x.y[i].z, we will:
                                       -- - add a WriteDynArray gate for x.y, limited to .z
                                       -- - update one of x's gates (the array gate)
                                       do i             <- nextInt
                                          (c4, [arr_n]) <- genExp c3 arr_e
                                          (c5, [idx_n]) <- genExp c4 idx_e
                                          let (ExpT arr_t _)  = arr_e
                                              total_blen = sum $ map snd blocs
                                              ctx        = mkCtx $
                                                           Gate i
                                                                arr_t
                                                                (WriteDynArray (fst $ head blocs,
                                                                                total_blen))
                                                                ([arr_n, idx_n] ++ gates)
                                                                [] []
                                          updateVar
                                            (applyWithDefault (splice (arr_off,1) [i])
                                                              [i])
                                            rv
                                                        `trace`
                                                        ("WriteDynArray: rv = " << rv
                                                         << "; arr_off=" << arr_off)
                                          return $ ctx & c5


      -- quite similar to the above. really need to structure this better
      s@(SAss lval@(EArr arr_e idx_e) val) ->
          do i                  <- nextInt
             type_table         <- getTypeTable
             let (rv,off)        = getRootvarOffset type_table getStrTLocs arr_e
             (c3, gates)        <- genExpWithDoc circ
                                                 (addOutputFlag rv)
                                                 val
                                                 lval
             (c4, [arr_n])      <- genExp c3 arr_e
             (c5, [idx_n])      <- genExp c4 idx_e
             let (ExpT arr_t _)  = arr_e
                 ctx             = mkCtx $ Gate i
                                                arr_t
                                                -- NOTE: writing -1 here to mean "the end"
                                                (WriteDynArray (0,
                                                                -1))
                                                ([arr_n, idx_n] ++ gates)
                                                [] []
             updateVar (applyWithDefault (splice (off,1) [i]) [i]) rv
             return $ ctx & c5


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
    where addOutputFlag var =
              case strip_var var of
                (VSimple "main") -> Just (\gate -> setFlags [Output] gate)
                _                -> Nothing


-- extract an array sub-expression from the given Exp
-- also return the list of field-locations (in bytes) of the array element type
-- which are covered by the whole expression
-- and also return the offset of the array expression under its root variable.
-- by example, say we have an expression x.y[i].z
-- then, the outputs are
-- ( the array expression x.y[i],
--   the offset of .y under x,
--   the (byte offset, byte len)'s of .z under x.y[i]
-- )
-- return Nothing if there is no array subexpression
extractEArr :: TypeTable -> Exp -> Maybe ( Exp,
                                           Int,
                                           [(Int,Int)]
                                         )

extractEArr tab (ExpT elem_t exp@(EArr arr_e idx_e)) =
    Just $ let (blocs,_)    = getTypByteLocs tab elem_t
                              -- here we need the offset in primitive types
               (rv,off)     = getRootvarOffset tab getStrTLocs arr_e
           in  (exp, off, blocs)

-- get a recursive answer and return just the slice of this field
extractEArr  tab e@(EStruct str_e idx) =
    -- this is in the Maybe monad
    do (arr_exp,arr_off,sublocs)   <- extractEArr tab str_e
       let (_,locs,_)               = getStrTParams tab str_e
           (off,len)                = locs !! idx
       return (arr_exp, arr_off,
               (take len $ drop off sublocs)
                    `trace`
                    ("extractEArr (" << e << ")" <<
                     "; sublocs=" << sublocs <<
                     "; off=" << off <<
                     "; len=" << len))

extractEArr tab (ExpT _ e)  = extractEArr tab e

-- if we hit a primitive expression, there's no array in the exp.
extractEArr _   _           = Nothing




-- see if this var is an output var, and if so remove the Output flag
-- on its current gates
-- Called when the location of a var is about to be updated
-- optionally an offset and length to limit the flag removal to some
-- of the gates, in case only part of a complex output var is being
-- modified.
checkOutputVars :: Circuit -> Var -> Maybe FieldLoc -> OutMonad Circuit
checkOutputVars c var mb_gate_loc
    | strip_var var == VSimple "main" =
        -- remove the output flags there
        do mb_vgates    <- getsVars $ maybeLookup var
           case mb_vgates of
             Nothing        -> return c
             Just vgates    ->
                 -- take a slice of the gates if mb_gate_loc is not Nothing
                 do let vgates' = case mb_gate_loc of
                                    Nothing          -> vgates
                                    (Just (off,len)) -> (take len $ drop off vgates)
                                                           `trace`
                                                           ("checkOutputVars (off,len)="
                                                            << (off,len) <<
                                                            "; vgates=" << vgates)

                        -- and update the Gate flags on those gates
                        c' = foldl (updateLabel (setFlags []))
                                   c
                                   vgates'
                    return c'
    | otherwise =
        return c `trace` ("checkOutputVars non-matching var: " << var)


-- update the label for a given node in a graph, if this node is present
updateLabel :: (Gr.DynGraph gr) => (a -> a) -> gr a b -> Gr.Node -> gr a b
updateLabel f gr node = let (mctx,gr')                  = Gr.match node gr
                        in  case mctx of
                              Nothing   -> gr
                              Just ctx  -> (tup4_proj_3 f ctx) & gr'


                            

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
              in  mkCtx (Gate i t Select src_gates [] [])


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
        [Im.BoolT          , Im.BoolT          ]    -> Im.BoolT
        [Im.IntT i1        , Im.IntT i2        ]    -> Im.IntT $ Im.BinOp Max i1 i2
        [t1@(Im.StructT _) , t2@(Im.StructT _) ]    -> t1 `trace`
                                                            ("getSelType got types " <<
                                                             t1 << " and " << t2)
        [a1@(Im.ArrayT _ _), a2@(Im.ArrayT _ _)]    -> a1 `trace` ("getSelType got types " <<
                                                                   a1 << " and " << a2)
    where getType (Gate _ t _ _ _ _) = t



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
                  

-- this one is a little nasty - it returns the expanded circuit, and
-- either the context for this expression, which can be processed and
-- added to the circuit by the caller, or the gate number where this
-- expression can be found (in case it was already in the circuit)
--
-- Need to be able to return a list of nodes, in the case of a struct
-- or array
--
-- also need to be able to return a list of newly generated Contexts,
-- for a ReadDynArray gate with all its following Slicer gates


genExp' :: Circuit -> Exp -> OutMonad (Circuit, (Either
                                                  [CircuitCtx]
                                                  [Gr.Node]))
genExp' c exp =
    case exp `trace` ("genExp' " << stripExpT exp) of
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
                              let ctx       = mkCtx $ Gate i VoidT (Bin op) [gate1, gate2] [] []
                              return (c2, Left [ctx])

      (UnOp op e1)      -> do i <- nextInt
                              (c1, [gate1]) <- genExp c   e1
                              let ctx       = mkCtx $ Gate i VoidT (Un op) [gate1] [] []
                              return (c1, Left [ctx])

      (EVar var)        -> do gates <- getsVars $
                                       fromJustMsg ("CircGen: Lookup Var " << var) .
                                       maybeLookup var
                              return (c, Right gates)

      (ELit l)          -> do i         <- nextInt
                              let ctx   = mkCtx (Gate i VoidT (Lit l) [] [] [])
                              return (c, Left [ctx])

      -- here we try to update the Typ annotation on the Gate
      -- generated recursively
      (ExpT typ e)      -> do (c', res) <- genExp' c e
                              case res of
                                (Left          [(c1, c2, gate,                 c3)]) ->
                                    let ctx' =  (c1, c2, setGateType typ gate, c3)
                                    in  return (c', Left [ctx'])
                                -- FIXME: can't deal with multiple contexts
                                -- for now
                                (Left ctxs) ->
                                    return (c', res)
                                (Right node)    ->
                                    return (c', res)

      (EStruct str_e idx) -> 
                           do type_table        <- getTypeTable
                              (c', gates)       <- genExp c str_e
                              let (_,locs,_)     = getStrTParams type_table str_e
                                  (off,len)      = locs !! idx
                              return (c', Right $ take len $ drop off gates)

      (EArr arr_e idx_e) ->
                           do (c1, arr_ns)  <- genExp c arr_e
                              let arr_n = case arr_ns of
                                            [arr_n] -> arr_n
                                            _       -> error ("Array got too many wires: " <<
                                                              arr_ns)
                              (c2, [idx_n]) <- genExp c1 idx_e
                              readarr_n     <- nextInt
                              type_table    <- getTypeTable
                                  -- get the array element type from the
                                  -- gate where the array now is
                              let (ArrayT elem_t _) = gateType $
                                                      fromJustMsg
                                                        "CircGen: get current array node" $
                                                      Gr.lab c2 arr_n
                                  ctx               = mkCtx (Gate readarr_n
                                                                  VoidT
                                                                  (ReadDynArray)
                                                                  [arr_n, idx_n]
                                                                  [] [])
                                  (e_locs,e_typs)   = getTypByteLocs type_table
                                                                     elem_t
                                  elem_count        = length e_locs
                              if (elem_count == 1)
                                     then -- easy, just need this gate
                                          return (c2, Left [ctx])
                                     else -- now we need a bunch of
                                          -- slicer gates
                                          do is <- replicateM elem_count nextInt
                                             let slicers = [Gate
                                                            i
                                                            t
                                                            (Slicer (off,len))
                                                            [readarr_n]
                                                            [] [] | i           <- is
                                                                  | (off,len)   <- e_locs
                                                                  | t           <- e_typs]
                                                 ctxs2   = map mkCtx slicers

                                             -- we'll add the ReadDynArray into the cct
                                             -- here, and not return it with the slicer
                                             -- contexts, as others should only look for
                                             -- vars at the slicers
                                                 c3 = ctx & c2
                                             return (c3, (Left ctxs2))

      (EStatic e)       -> genExp' c e

      e                 -> error $ "CircGen: unknown expression " << e



-- make a graph Context for this operator, node number and source
-- gates
mkCtx :: Gate -> CircuitCtx
mkCtx g@(Gate node _ _ srcs _ _) = let ctx = (map uAdj srcs,
                                              node,
                                              g,
                                              []) -- no outgoing edges
                                    in ctx



{-
-- get the ordered list of types in this type (complex or not)
getStructFieldTypes :: TypeTable -> Typ -> [Typ]
getStructFieldTypes tab t =
    let t_full = expandType tab t
    in  f t_full
    where f (StructT fields)    = concatMap (f . snd) fields
          f t                   = [t]


-- return the Struct type's list of types and offsets.
-- WARN: uses GHC extension "parallel list comprehension"
getStructFieldLocs :: TypeTable -> Typ -> [(Int, Typ)]
getStructFieldLocs tab t = [ (off, t)  | t    <- getStructFieldTypes tab t
                                       | off  <- [0..]]



-}

-- similar to above but the locations (offset-length) are in bytes
getTypByteLocs :: TypeTable -> Typ -> ([FieldLoc], [Typ])
getTypByteLocs tab t =
    let t_full = expandType tab t
    in case t_full of
         (StructT (fields,_,bytelocs))  ->
             let types          = map snd fields
             in  (bytelocs, types)
         t                              ->
             ([(0, tblen t)], [t])

-- make an unlabelled edge
uAdj n = ((), n)



-- extract the params of main() from a Prog
-- FIXME: Func does not now hang on to the types of the function
-- parameters, which we do need here.
-- for now invent a type :)
extractInputs (Prog pname (ProgTables {funcs=fs})) =
    let (Func _ _ t form_args stms) = fromJust $ Map.lookup "main" fs
    in  form_args



--------------------
-- state and state access functions
--------------------

-- the state in these computations:
type MyState = ([VarTable],     -- stack of table Var -> [Node]
                Int,            -- a counter to number the gates
                                -- sequentially
                TypeTable       -- the type table is read-only, so
                                -- could be in a Reader, but easier to
                                -- stick it in here 
               )
{-
instance St.MonadState Int (St.State MyState) where
    get = getInt
    put = tup3_get2 >>= St.put
-}



getsVars        f = St.gets $ f . tup3_get1
getsTypeTable   f = St.gets $ f . tup3_get3

-- modify the various parts of the state with some function
modifyVars = St.modify . tup3_proj1
modifyInt  = St.modify . tup3_proj2


getTypeTable = getsTypeTable id
getInt = St.gets tup3_get2
getVars = getsVars id


-- update the location of 'var' by some function of (Maybe [Node])
-- the new value always goes into the top-most scope
-- we always use this, even if the var is certain not to be present (eg. during static
-- initialization), for greater uniformity.
updateVar :: (Maybe [Gr.Node] -> [Gr.Node]) -> Var -> OutMonad ()
updateVar f var = modifyVars $ \maps -> let curr    = maybeLookup var maps
                                            new     = f curr
                                            maps'   = modtop (Map.insert var new) maps
                                        in  maps'


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
                                            << op << " [:" << typ << ":] "
                                            << srcs
                                            << (if not (null doc) then " ~" << doc << "~"
                                                                  else "")


instance Show GateFlags where
    show Output = "o"

-- this requires -fallow-overlapping-instances to GHC, as we already
-- have
-- (Show a) => instance Show [a]
instance Show [GateFlags] where
    show flags = concatMap show flags


instance Show Op where
    show (Bin op)           = show op
    show (Un  op)           = show op
    show Input              = "in"
    show Select             = "sel"
    show (Lit l)            = "lit " << l
    show ReadDynArray       = "readarr"
    show (WriteDynArray
             (off,len))     = "writearr " << len << "@" << off
    show (Slicer (off,len)) = "slice " << len << "@" << off

showCct c = Graphviz.graphviz' c




-------------------------------------
-- some tests
-------------------------------------

testNextInt = let startState    = ([Map.empty], 0, Map.empty)
                  (out,st)      = St.runState test_f startState
              in  (out,st)
    where test_f = do is <- replicateM 5 nextInt
                      return (is)
