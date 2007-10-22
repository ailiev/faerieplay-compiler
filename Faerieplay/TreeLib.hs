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

module Faerieplay.TreeLib where

import qualified Data.Tree                      as Tree

import Control.Monad.Identity (runIdentity)

import Prelude hiding (mapM,map)
import qualified Prelude                        (mapM,map)


----------------------
-- Tree functions
----------------------

-- | map a monadic function on all the nodes of a 'Tree'.
mapM :: (Monad m) => (a -> m b) -> Tree.Tree a -> m (Tree.Tree b)
mapM f   (Tree.Node l ts) = do l'  <- f l
                               ts' <- Prelude.mapM (mapM f) ts -- works for ts == []
                               return $ Tree.Node l' ts'

-- | map a normal function over a tree
map :: (a -> b) -> Tree.Tree a -> Tree.Tree b
map f t = runIdentity $ mapM (return . f) t


-- | iterate a function which produces a finite list, to produce a Tree.
-- Similar to Tree.unfoldTree, except here we start the tree with the seed value.
iterate :: (a -> [a]) -> a -> Tree.Tree a
iterate f = Tree.unfoldTree (\x -> (x, f x))


-- | prune a tree up to and including depth 'd' (ie. the root is always kept)
prune :: (Ord a, Num a) => a -> Tree.Tree b -> Tree.Tree b
prune d (Tree.Node r subs)
    | d <= (fromInteger 0)  = Tree.Node r []
    | otherwise             = Tree.Node r (Prelude.map (prune (d-1)) subs)

-- | The leaves of a 'Tree.Tree' in left-to-right order
leaves :: Tree.Tree a -> [a]
leaves (Tree.Node x [])         = [x]
leaves (Tree.Node _ subtrees)   = concatMap leaves subtrees
