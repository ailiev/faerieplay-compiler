module GraphLib
(
 contexts,
 start_ctxs,
 vmap_ordered,
 showGraph
, fwd, back
, pruneGraph
)
where


import          Maybe                           (fromMaybe, maybeToList, isJust)
import          List                            (sortBy, partition)

import              Array                       (Array, array, listArray, accum, assocs)

import qualified    Data.Tree                   as Tree
import qualified    Data.IntSet                 as IS


import Data.Graph.Inductive.Graph               ((&))
import qualified Data.Graph.Inductive.Graph     as Gr
import qualified Data.Graph.Inductive.Basic     as GrBas
import qualified Data.Graph.Inductive.Graphviz  as Graphviz
import qualified Data.Graph.Inductive.Tree      as TreeGr
import qualified Data.Graph.Inductive.Query.DFS as GrDFS
import qualified Data.Graph.Inductive.Query.BFS as GrBFS

import SashoLib                                 (comp2_1,expand,proj_tup2,iterateTree,pruneTree,(<<),
                                                StreamShow(..))
import Common                                   (trace)




-- | Same as Gr.gmap, but with specified order of nodes
vmap_ordered :: (Gr.DynGraph gr, StreamShow c, StreamShow a) =>
             (Gr.Context a b -> Gr.Context c d) -- ^ The function on Gr.Context
          -> [Gr.Node]          -- ^ The order; all nodes should be in here
          -> gr a b
          -> gr c d
vmap_ordered ctx_f nodes g  = {-
                              let ctxs' = GrBFS.bfsnWith ctx_f starts g
                              in  (Gr.buildGr $ ctxs')
                                  `trace`
                                  ("vmap_bfs: ctxs'=\n" << map ((++ "\n") . showCtxDbg) ctxs'
                                  ) -}
                              let ctxs          = map (Gr.context g) nodes
                                  ctxs'         = map ctx_f ctxs
                                  -- bring indegree 0 contexts to the front, to try to
                                  -- make sure the graph building succeeds.
                                  (id0,rest)    = List.partition ((== 0) . Gr.indeg') ctxs'
                                  ctxs''        = id0 ++ rest
                              in  myBuildGr ctxs''
{-
                                  `trace`
                                  ("\nvmap_bfs: ctxs=\n" << map ((++ "\n") . showCtxDbg) ctxs
                                   << "\nvmap_bfs: ctxs''=\n" << map ((++ "\n") . showCtxDbg) ctxs''
                                  )
-}


-- showCtxDbg :: CircuitCtx -> String
showCtxDbg ctx@(ins,n,gate,outs) =
    ("Node " << n
     << "; ins=" << map snd ins
     << "; gate=" << gate
     << "; outs=" << map snd outs)


-- | Build a 'Gr.DynGraph' from 'Gr.Context's using 'foldl' rather than 'foldr'; ie.
-- Contexts are added in the order given.
myBuildGr :: (Gr.DynGraph gr, StreamShow a)   => [Gr.Context a b] -> gr a b
myBuildGr = foldl (\gr ctx -> ctx & gr
--                               `trace`
--                               ("myBuildGr: Adding in ctx " << showCtxDbg ctx)
                  )
                  Gr.empty



-- | Merge two graphs together, context by context. Complexity is function of the smaller
-- | graph's size.

-- we make sure to do it in an order which mimizes work, ie. so the smaller graph is
-- folded into the larger.
merge :: Gr.DynGraph gr =>
         gr a b -> gr a b -> gr a b
merge ga gb = let [g1,g2] = sortBy (compare `comp2_1` Gr.noNodes) [ga,gb]
              in  foldl (flip (&)) g2 $ contexts g1


-- | The list of contexts in a graph.

-- FIXME: It is apparently very difficult to do this correctly. This implementstion
-- probably takes VlogV time for a Tree-based graph. The other two attempts (commented
-- out) do not return the whole Contexts - some have edges missing, probably because graph
-- traversal is implemented by removing Contexts, edges to which then disappear from
-- subsequently traversed Contexts.
contexts :: Gr.Graph gr => gr a b -> [Gr.Context a b]
contexts g = {- GrBas.gsel (const True) g -}
             {-let starts = map Gr.node' $ GrBas.gsel ((== 0) . Gr.indeg') g
               in  GrBFS.bfsnWith id starts g -}
             map (Gr.context g) $ Gr.nodes g


-- | The start 'Gr.Context's in a graph, ie. which have no in-edges
start_ctxs :: Gr.Graph gr => gr a b -> [Gr.Context a b]
start_ctxs = GrBas.gsel ((== 0) . Gr.indeg')


-- * 'Read' and 'Show' instances for 'Gr.Graph'

-- a Graph as an adjacency list of contiguous, labelled nodes.
-- useful because has a derived instance of Show and Read, whereas the Graph.Inductive
-- graphs don't
type EdgeList elab = [(Gr.Node,elab)]
data AdjList vlab elab = AdjList { v  :: (Gr.Node,vlab),
                                   es :: EdgeList elab
                                 }
    deriving (Show,Read)


-- use an Array of vertex number to (vlab, EdgeList) to store the graph, before exporting
-- the Array to a list.
graph2adjList gr = let -- Start the array with blanks at all slots
                       graphArr'' :: Array Gr.Node (Maybe (vlab, EdgeList elab))
                       graphArr'' = listArray (Gr.nodeRange gr) (repeat Nothing)
                       -- add  an entry for every node, and its label
                       graphArr' = accum (\_ v_new -> Just v_new)
                                         graphArr''
                                         [ (src `trace` ("accum1: src=" << src),
                                            (vlab,[])
                                           )
                                           | (src,vlab) <- Gr.labNodes gr
                                         ]
                       -- and now add in all the edge info - destinations and labels.
                       -- leave the node labels untouched (via. 'const')
                       graphArr  = accum (\mb_b (_,c2) -> do (b1,b2)   <- mb_b
                                                             return (b1, b2 ++ c2))
                                         graphArr'
                                         [ ( src `trace` ("accum2: src=" << src),
                                             (undefined, -- will not be touching the
                                                         -- vertex label
                                              [(dest,elab)])
                                           )
                                           | (src,dest,elab) <- Gr.labEdges gr
                                         ]
                   in [ AdjList { v     = (i_v, vlab),
                                  es    = edgeList }
                        | (i_v, (Just (vlab, edgeList)))  <- filter (isJust . snd) $ assocs graphArr
                      ]
                  

adjList2graph adjs = let nodes = [(v,vlab) | (v,vlab) <- map v adjs]
                         adjLists = [(s, es)   | (s,_)  <- map v adjs
                                               | es     <- map es adjs
                                    ]
                         edges = [ (i_s,i_d,elab) | (i_s, (i_d,elab)) <- expand adjLists ]
                     in Gr.mkGraph nodes edges




-- | Convert a Gr.Graph to a String via the AdjList format; can be read back in with the
-- 'Read' instance.
showGraph g = show $ graph2adjList g

-- | reading graphs via AdjList format
instance (Read vlab, Read elab, Gr.Graph gr) => Read (gr vlab elab)
    where readsPrec x s = let parses = readsPrec x s
                          in
                            map (proj_tup2 (adjList2graph, id)) parses




-- * Fold directions

type GFoldDir a b = (Gr.Context a b) -> [Gr.Node]

-- | Forward, ie. following out-edges
fwd :: GFoldDir a b
fwd (_,_,_,outs)    = map snd outs

-- | Backwards, following in-edges
back :: GFoldDir a b
back (ins,_,_,_)    = map snd ins



-- * Misc

-- | return a sub-graph of 'gr', with all nodes in the direction given by 'dir', within
-- 'dist' of 'center'.
pruneGraph ::   (Gr.Graph gr) =>
                (Gr.Context v e -> [Gr.Node])
             -> Gr.Node
             -> Int
             -> gr v e
             -> gr v e
pruneGraph dir center dist gr =
    let paths       = iterateTree (dir . Gr.context gr) center
        shortPaths  = pruneTree dist paths
        wantNodes   = Tree.flatten $ shortPaths
        -- use a Set data structure for efficiency here, as List.\\ is O(n * m)
        delNodes    = IS.elems $ IS.fromList (Gr.nodes gr) `IS.difference`
                                 IS.fromList wantNodes
    in
      Gr.delNodes delNodes gr



-- | a test function, to do a very simple gfold and build a spanning 'Tree'
-- in this case, the type vars for gfold are:
-- c = [Tree.Tree a]
-- d = Tree.Tree a
makeSimpleTree :: (Gr.Graph gr, Show a) =>
                  [Gr.Node] -> GFoldDir a b -> gr a b -> [Tree.Tree a]
makeSimpleTree starts dir g =
    GrBas.gfold dir
                (  \ctx trees     -> Tree.Node (Gr.lab' ctx) trees)
                ( (\mb_tree trees -> maybeToList mb_tree ++ trees)   ,  [] )
                starts
                g



-- result is [Gr.Node] in some order
gfold_order starts dir g = GrBas.gfold dir f_depth (f_breadth, []) starts g
    where f_depth ctx rec_ns        = Gr.node' ctx : rec_ns
          f_breadth mb_ns these_ns  = these_ns ++ (fromMaybe [] mb_ns)
