-- a tree class
-- awaiting understanding of universal quantification for types...

-- needed functions:
-- children (t)    -- list of children
-- nil (t)         -- sub-leaf node?
-- node (t)        -- the actual node of this tree


-- use multi-parameter type classes
class Tree t a where
    children        :: t a -> [t a]
    nil             :: t a -> Bool
    node            :: t a -> a


------
-- and some tree functions
------

--                     fnode            fcons            nil  tree
redtree :: (Tree t) => (a -> b -> c) -> (c -> b -> b) -> b -> t a ->  c
redtree fnode fcons a t = fnode (node t) (redtree' fnode fcons a (children t))
-- redtree' takes (fnode, fcons, start) and a list of trees
--          returns 
    where redtree' :: (Tree t) => (a -> b -> c) -> (c -> b -> b) -> b -> [t a]  -> b
          redtree' fnode fcons a (t:ts) = fcons (redtree fnode fcons a t) (redtree' fnode fcons a ts)
          redtree  _     _     a []     = a


-- example
labels :: (Tree t) => t a -> [a]
labels = redtree (:) (++) []
