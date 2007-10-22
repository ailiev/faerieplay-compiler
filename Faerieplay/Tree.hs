--
-- Circuit compiler for the Faerieplay hardware-assisted secure
-- computation project at Dartmouth College.
--
-- Copyright (C) 2003-2007, Alexander Iliev <sasho@cs.dartmouth.edu> and
-- Sean W. Smith <sws@cs.dartmouth.edu>
--
-- All rights reserved.
--
-- This code is released under a BSD license.
-- Please see LICENSE.txt for the full license and disclaimers.
--

-- a tree class
-- awaiting understanding of universal quantification for types...

module Faerieplay.Tree where


-- needed functions:
-- children (t)    -- list of children
-- nil (t)         -- sub-leaf node?
-- node (t)        -- the actual node of this tree


-- use multi-parameter type classes
class LabTree t a where
    children            :: t a -> [t a]
--    nil             :: t a -> Bool
    nodeExtr            :: t a -> a
    nodeCons            :: a -> [t a] -> t a

--    nil t           = False     -- default

class MyTree t where
    recons      :: t -> [t] -> t
    kids    :: t -> [t]
--    isLeaf      :: t -> Bool


mapTree f t = let cs = map f (kids t)
                  t_new = f t
              in recons t_new cs

                  

data Exp = EPlus Exp Exp
         | EInt Int

data Stm = SAss Exp Exp
         | SIf Exp Stm


data SynTree = TExp Exp
             | TStm Stm

instance MyTree SynTree where
    recons (TExp e) ts = case (e,ts) of
                                     ( (EPlus _ _) , [(TExp e1),(TExp e2)]) -> (TExp $ EPlus e1 e2)
    recons (TStm s) ts = case (s,ts) of
                                     ( (SAss _ _) , [(TExp e1),(TExp e2)] ) -> (TStm $ SAss e1 e2)
                                     ( (SIf _ _)  , [(TExp e1),(TStm s1)] ) -> (TStm $ SIf e1 s1)
    kids (TExp (EPlus e1 e2)) =   [TExp e1, TExp e2]
    kids (TExp (EInt i))        = []
    kids (TStm (SAss e1 e2))  = [TExp e1, TExp e2]
    kids (TStm (SIf e1 s1))   = [TExp e1, TStm s1]


egTree = (TStm 


data BinTree a = Node a (BinTree a) (BinTree a)
               | Leaf a
    deriving (Show)



instance (LabTree BinTree) a where
    children (Node _ b1 b2)     = [b1,b2]
    children (Leaf _)           = []

    nodeExtr (Node x _ _)           = x
    nodeExtr (Leaf x)               = x

    nodeCons x []                  = Leaf x
    nodeCons x [c1,c2]          = Node x c1 c2
                              


------
-- and some tree functions
------

--                           fnode            fcons           base  tree
redtree :: (LabTree t a) => (a -> b -> c) -> (c -> b -> b) -> b -> t a ->  c
redtree fnode fcons base t = fnode (nodeExtr t) (redtree' fnode fcons base (children t))
-- redtree' takes (fnode, fcons, start) and a list of trees
--          returns 
    where redtree' :: (LabTree t a) => (a -> b -> c) -> (c -> b -> b) -> b -> [t a]  -> b
--           redtree' fnode fcons base (t:ts)
--               | nil(t)                     = fcons base (redtree' fnode fcons base ts)
          redtree' _     _     base []     = base
          redtree' fnode fcons base (t:ts) = fcons (redtree fnode fcons base t) (redtree' fnode fcons base ts)


-- example
labels :: (LabTree t a) => t a -> [a]
labels = redtree (:) (++) []

maptree :: (LabTree t a) => (a -> a) -> t a -> t a
maptree f = redtree (nodeCons . f) (:) []

testBinTree = Node 2 (Leaf 3) (Node 5 (Leaf 3) (Leaf 4))

main = print $ labels testBinTree
