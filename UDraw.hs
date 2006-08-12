module UDraw (
              Term
             , makeTerm
             , TermId
             , Kind
             , DocAble(..)
             , showGraph
             , pruneGraph
             , fwd, back
             ) where


import              SashoLib
import              Common                          (trace)

import              Maybe                           (maybeToList, fromJust, isJust)
import              List                            ((\\), nub)
import qualified    List
import              Array                           (array, listArray, accum, assocs)
import              Char                            (isSpace)

import              Control.Monad.Error             (MonadError(..))

import qualified    Data.Graph.Inductive.Example    as GrEg
import qualified    Data.Graph.Inductive.Graph      as Gr
import qualified    Data.Graph.Inductive.Basic      as GrBas
import qualified    Data.Graph.Inductive.Graphviz   as Graphviz
import qualified    Data.Graph.Inductive.Tree       as TreeGr

import qualified    Data.Tree                       as Tree
import qualified    Data.Set                        as Set
import qualified    Data.IntSet                     as IS

import qualified    Control.Monad.State             as St

import              Text.PrettyPrint                as PP

import qualified    Debug.Trace                     as Trace



data Kind = Node | Edge

data TermId = NodeId Gr.Node
            | EdgeId (Gr.Node,Gr.Node,Int) -- the Int is a copy number in case of multi-
                                           -- edges
    deriving (Eq,Ord)              -- needed for Set


-- the label of a single node in the term graph
data NodeLabel id_t = L { tid      :: id_t,
                          kind     :: Kind,
                          lclass   :: String,
                          attribs  :: [(String,String)] }
                    | R id_t -- a reference

type Term id_t = Tree.Tree (NodeLabel id_t)


-- | Make a UDrawGraph Term format of a Graph.
makeTerm :: (Gr.Graph gr) =>
            (a -> String)
         -> (a -> [(String,String)])
         -> [Gr.Node]
         -> gr a b
         -> [Term TermId]
makeTerm f_class                -- ^ Function to get the class of a Node
         f_attribs              -- ^ Function to get the attributes of a Node
         starts                 -- ^ The starting nodes
         g                      -- ^ The graph
    = GrBas.gfold fwd (mkTerm g f_class f_attribs) (doTerm, []) starts g
{-
    where 
          genRefs ts    = St.evalState (genRefsSt ts) (Set.empty :: Set.Set TermId)
          genRefsSt     = mapM (mapTreeM doNode)
          doNode r@(R _)            = error "makeTerm.doNode should not see an R"
          doNode l@(L { tid = id }) = do ids <- St.get
                                         if Set.member id ids
                                              then -- already have the original, return a
                                                   -- reference 
                                                  do return $ R id
                                              else -- first time seeing this, add to set
                                                   -- and return original
                                                  do St.put $ Set.insert id ids
                                                     return l
-}

-- | a test function, to do a very simple gfold and build a spanning 'Tree'
makeSimpleTree :: (Gr.Graph gr, Show a) => [Gr.Node] -> gr a b -> [Tree.Tree a]
makeSimpleTree starts g = GrBas.gfold fwd
                                      (  \ctx trees     -> Tree.Node (Gr.lab' ctx) trees)
                                      ( (\mb_tree trees -> maybeToList mb_tree ++ trees)   ,  [] )
                                      starts g


-- | graph exploration direction: forward
fwd (_,_,_,outs) = map snd outs

-- | graph exploration direction: backwards
back (ins,_,_,_) = map snd ins


-- deal with one Graph Context, and the sub-terms generated for (some of) its out-edge
-- nodes
-- NOTE: use the original graph to get all the outgoing edges for the current node 'n'
mkTerm :: (Gr.Graph gr) =>
          gr a b
       -> (a -> String)
       -> (a -> [(String,String)])
       -> Gr.Context a b
       -> [Term TermId]
       -> Term TermId
mkTerm orig_gr f_class f_attribs
       (_,n,label,outs) ts = -- #outs >= #ts!
    let -- Edges to nodes which had trees generated in the recursive call
        recNodes            = (map (\id -> let NodeId n = id in n) $
                               map (tid . Tree.rootLabel) ts)
                                  {- `trace` ("mkTerm node = " ++ show n
                                           ++ "; outs=" ++ show (map snd outs)) -}
                  
        e_terms = [Tree.Node { Tree.rootLabel = L { tid      = EdgeId (n,dest,0),
                                                    kind     = Edge,
                                                    lclass   = "",
                                                    attribs  = [] },
                               Tree.subForest = [dest_term] }
                   | dest       <- recNodes
                   | dest_term  <- ts]

        -- Edges to Nodes which were explored already (grey/black??), had no trees
        -- generated here, so have them point to a Node reference.
        --
        -- first the original context for this node, with all its outgoing edges
        origDests = let (_,_,_,origEs) = Gr.context orig_gr n
                    in map snd origEs
        e_ref_terms = [Tree.Node { Tree.rootLabel = L { tid     = EdgeId (n,dest,0),
                                                        kind    = Edge,
                                                        lclass  = "",
                                                        attribs = [] },
                                   Tree.subForest = [Tree.Node (R $ NodeId dest) []] }
                       | dest   <- origDests \\ recNodes
                                    `trace` ("UDraw.mkTerm " << n
                                             << ": origOuts=" << origDests
                                             << "; recNodes=" << recNodes)
                      ]
        all_e_terms = numberDups $ e_terms ++ e_ref_terms

        -- and this Node
        n_term  =  Tree.Node { Tree.rootLabel = L { tid      = NodeId n,
                                                    kind     = Node,
                                                    lclass   = f_class label,
--                                                    attribs  = [("OBJECT", show label)]
                                                    attribs  = f_attribs label
                                                  },
                               Tree.subForest = all_e_terms }

    in
      n_term


-- applied to aggregate the results of mkTerm coming off one Node
doTerm mb_term terms = Trace.trace ("doTerm " ++ show mb_term ++ ", " ++ show terms) $
                       maybeToList mb_term ++ terms


numberDups = mapAccumDupsBy ((==) `comp2_1` tid . Tree.rootLabel) addNum 0
    where addNum n t@(Tree.Node l@(L {tid = EdgeId (s,d,0)}) subs)
              | n > 0       = ( n+1, (Tree.Node (l {tid = EdgeId (s,d,n)}) subs) )
              | otherwise   = ( n+1, t )
          addNum n t        = error $ "UDraw.numberDups: Unexpected node "
                                       ++ (show $ Tree.rootLabel t)

{-
dups2refs ls = mapDupsBy ((==) `comp2_1` tid . Tree.rootLabel) mkRef ls
    where mkRef   (Tree.Node (L {tid = id}) subs) = Tree.Node (R $ id) subs
          mkRef t@(Tree.Node (R _)          subs) = error "UDraw.dups2refs: Unexpected Ref in input list"
-}



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
                       graphArr'' = listArray (Gr.nodeRange gr) (repeat Nothing)
                       -- add  an entry for every node, and its label
                       graphArr' = accum (\_ v_new -> Just v_new) graphArr''
                                            [ (src, (vlab,[])) | (src,vlab) <- Gr.labNodes gr
                                            ]
                       -- and now add in all the edge info - destinations and labels.
                       -- leave the node labels untouched (via. 'const')
                       graphArr  = accum (\mb_b (_,c2) -> do (b1,b2)   <- mb_b
                                                             return (b1, b2 ++ c2))
                                         graphArr'
                                         [ ( src, (undefined, [(dest,elab)]) )
                                               | (src,dest,elab) <- Gr.labEdges gr]
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


-- return a sub-graph of 'gr', with all nodes in the direction given by 'dir', within
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



showGraph g = show $ graph2adjList g

-- reading graphs via AdjList format
instance (Read vlab, Read elab, Gr.Graph gr) => Read (gr vlab elab)
    where readsPrec x s = let parses = readsPrec x s
                          in
                            map (proj_tup2 (adjList2graph, id)) parses

{-
-- Read instance for graphs
-- all these reads functions are in the List Monad

-- read a line of text into a Graph Context
readsCtx :: (Read v, Read e) => ReadS (Gr.Context v e)
readsCtx s  = do (v_i,s1)   <- reads s -- node number
                 let (':':s15)  = s1 `trace` ("s1=" ++ s1) -- a colon
                 (v_lab,s2) <- reads s15 `trace` ("s15=" ++ s15) -- node label
                 (_    ,s3) <- readsSymbol "->" s2 `trace` ("s2=" ++ s2) -- an arrow
                 (outs,s4)  <- reads s3 `trace` ("s3=" ++ s3) -- out-edges
                 return ( ([],v_i,v_lab,outs), skipws s4 ) `trace` ("s4=>" ++ s4 ++ "<=")

-- read a list of Contexts, till the input runs out
readsCtxs :: (Read v, Read e) => ReadS [Gr.Context v e]
readsCtxs s = ( do (ctx, s1)    <- readsCtx s
                   (ctxs, s2)   <- readsCtxs s1 `trace` ("readCtxs read in ctx " ++
                                                         let (_,n,_,_)=ctx in show n)
                   return (ctx:ctxs, skipws s2)
              )
              `catchError`
              ( const $ return ([],s) )
              -- NOTE: this will return [] if no Context is
              -- to be seen, and so the recursion will end
{-
                 case ctx of
                   ([],-1,_,[]) -> return ([], s)
                   _            -> do (ctxs, s2)  <- readsCtxs s1 `trace` ("readCtxs read in ctx " ++
                                                                           let (_,n,_,_)=ctx in show n)
                                      return (ctx:ctxs, skipws s2)
-}


readsGraph :: (Read v, Read e, Gr.Graph gr) => ReadS (gr v e)
readsGraph s = do (ctxs, s1)    <- readsCtxs s
                  let nodes     = [(v_i,v_lab)      | (_,v_i,v_lab,_) <- ctxs]
                      edgeLists = [(v_i,outs)       | (_,v_i,_,outs)  <- ctxs]
                      edges     = [(s_i,d_i,e_lab)  | (s_i, (e_lab, d_i)) <- expand edgeLists]
                      g         = Gr.mkGraph nodes edges
                  return (g, skipws s1 )

instance (Read vlab, Read elab, Gr.Graph gr) => Read (gr vlab elab)
    where readsPrec _ = readsGraph


readsSymbol sym s = if sym `List.isPrefixOf` s
                    then [( (), drop (length sym) s )]
                    else []

skipws = dropWhile isSpace

-}




-- | Take an assoc list of (key, values), and expand it to a (longer) list of
-- (key,value), where each 'values' has been expanded to one value per element.
expand :: [(a, [b])] -> [(a,b)]
-- use foldr to avoid quadratic blowup with (++)
expand xs = foldr f [] xs
    where f (a,bs) dones = [(a,b) | b <- bs] ++ dones





{-
type AdjList = [Gr.Node]        -- list of target vertices
type AdjArr lab = Array Gr.Node (lab, AdjList)

foldAdjArr ::
    ( (lab,AdjList) -> c -> d) ) -- ^ Given a vertex (label and adj list), a 'c' from a
                                 -- recursive call to  'foldAdjArr', produce some 'd'
    -> (d -> c -> c)             -- ^ Fold along the d's produced by the d-function,
                                 -- starting with the 'c'
    -> [Gr.Node]                -- The starting vertices
    -> c                        -- accumulator
    -> AdjArr lab
    -> c

-}




----------------------
-- Machinery to print a UDrawTerm, using the Text.PrettyPrint library
----------------------

instance Show TermId where
    showsPrec x (NodeId n)      = ("Node " ++) . showsPrec x n
    showsPrec x (EdgeId (s,d,v))= ("Edge " ++) . showsPrec x s . ("->" ++) . showsPrec x d
                                  . (if v>0
                                     then (',':) . showsPrec x v
                                     else id)

-- quick and minimal for debuging
instance Show (NodeLabel TermId) where
    show (L tid kind lclass attribs)    = "L " ++ show tid
    show (R tid)                        = "R " ++ show tid


instance DocAble Kind where
    doc Node = PP.text "n"
    doc Edge = PP.text "e"


cNEST_DEPTH = 1

instance (Show id_t) => DocAble (Term id_t) where
    doc (Tree.Node (L id kind lclass attribs) children) =
        (PP.text "l"
              <> PP.lparen
                  <> quote (show id)
                  <> PP.comma
                  <> doc kind
                  <> PP.lparen
                      <> quote lclass
                      <> PP.comma
                      <> termList (map docAttrib attribs)
                      <> PP.comma)
        $$
        -- the children. Edges should have exaclty one child, and need to be printed
        -- without surrounding []
        (nest cNEST_DEPTH $ case kind of Node -> termList (map doc children)
                                         Edge -> let [child] = children
                                                 in  doc child)
        $$
        (PP.rparen <> PP.rparen)

    doc (Tree.Node (R id) children) =                -- a reference
        let [] = children       -- ASSERT, no children for a reference
        in
          (PP.text "r" <> PP.parens (quote $ show id))



instance (DocAble term) => DocAble [term] where
    doc = termList . map doc


docAttrib (key,val) = PP.text "a" <>
                      PP.parens (quote key <> PP.comma <> quote val)

quote = PP.doubleQuotes . PP.text

termList = PP.brackets . PP.vcat . PP.punctuate PP.comma



-- LIB:
-- convert a value to a Doc for pretty-printing.
class DocAble a where
    doc     :: a -> PP.Doc




------------------------
-- test drivers
------------------------

-- NOTE: the starting vertices are chosen with the test graph in mind.
testConvert = let g = GrEg.dag4
              in
                PP.render $ doc $ makeTerm (const "node")
                                           (\l -> [("OBJECT", show l)])
                                           [1]
                                           g

