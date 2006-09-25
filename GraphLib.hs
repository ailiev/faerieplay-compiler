module GraphLib where

import          Maybe                           (fromMaybe, maybeToList)
import          List                            (sortBy)

import qualified    Data.Tree                   as Tree


import Data.Graph.Inductive.Graph               ((&))
import qualified Data.Graph.Inductive.Graph     as Gr
import qualified Data.Graph.Inductive.Basic     as GrBas
import qualified Data.Graph.Inductive.Graphviz  as Graphviz
import qualified Data.Graph.Inductive.Tree      as TreeGr
import qualified Data.Graph.Inductive.Query.DFS as GrDFS
import qualified Data.Graph.Inductive.Query.BFS as GrBFS

import SashoLib                                 (comp2_1)

-- | Same as Gr.gmap, but with specified strarting nodes, and a specified direction. The
-- direction is specified by a fun
ordered_map :: Gr.DynGraph gr =>
             (Gr.Context a b -> Gr.Context c d) -- ^ The function on Gr.Context
          -> [Gr.Node]          -- ^ The starting nodes
          -> (Gr.Context a b -> [Gr.Node]) -- ^ The direction of mapping, eg
                                     -- forward = \(_,_,_,outs) -> outs
          -> gr a b
          -> gr c d
ordered_map ctx_f starts dir g = GrBas.gfold dir f_depth (f_breadth, Gr.empty) starts g
-- from the type of gfold, here we have the values of type vars c (c_gr) and d (d_gr):
-- c_gr = gr c d
-- d_gr = gr c d
    where
      -- this will get an empty rec_gr if it is a terminal vertex (no outgoing edges)
      f_depth ctx rec_gr    = let ctx' = ctx_f ctx
                              in  ctx' & rec_gr
      f_breadth mb_gr gr    = maybe (gr)
                                    (\gr' -> merge gr gr')
                                    mb_gr


-- | Merge two graphs together, context by context. Complexity is function of the smaller
-- | graph's size.

-- we make sure to do it in an order which mimizes work, ie. so the smaller graph is
-- folded into the larger.
merge :: Gr.DynGraph gr =>
         gr a b -> gr a b -> gr a b
merge ga gb = let [g1,g2] = sortBy (compare `comp2_1` Gr.noNodes) [ga,gb]
              in  foldl (flip (&)) g2 $ contexts g1


-- | The list of contexts in a graph.

-- Did not see any better way to do this in the API.
contexts :: Gr.Graph gr => gr a b -> [Gr.Context a b]
contexts = GrBas.gsel (const True)

-- | The start 'Gr.Context's in a graph, ie. which have no in-edges
start_ctxs :: Gr.Graph gr => gr a b -> [Gr.Context a b]
start_ctxs = GrBas.gsel ((== 0) . Gr.indeg')



-- * Fold directions

type GFoldDir a b = (Gr.Context a b) -> [Gr.Node]

-- | Forward, ie. following out-edges
fwd :: GFoldDir a b
fwd (_,_,_,outs)    = map snd outs

-- | Backwards, following in-edges
back :: GFoldDir a b
back (ins,_,_,_)    = map snd ins


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
