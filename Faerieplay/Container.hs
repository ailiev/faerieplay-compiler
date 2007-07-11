{-# OPTIONS_GHC -fglasgow-exts #-}
-- -fglasgow-exts: for multi-param type classes

module Faerieplay.Container where


import List (union)
import qualified Data.IntSet                    as IS
import qualified Data.Set                       as Set


-- a set class, without mapping
-- NOTE: multi-parameter type class.
class (Eq a) => Container c a | c -> a where
    insert      :: a -> c -> c
    member      :: a -> c -> Bool
    union       :: c -> c -> c
    toList      :: c -> [a]
    fromList    :: [a] -> c
    empty       :: c
    singleton   :: a -> c


instance (Eq a) => Container ([a]) a where
    insert      = (:)
    member      = elem
    union       = List.union
    toList      = id
    fromList    = id
    empty       = []
    singleton x = [x]


instance (Ord a) => Container (Set.Set a) a where
    insert      = Set.insert
    member      = Set.member
    union       = Set.union
    toList      = Set.toList
    fromList    = Set.fromList
    empty       = Set.empty
    singleton   = Set.singleton


-- NOTE: this instance would not be possible if Container class is not defined with 2
-- params, ie. if we had
-- class Container c where
--     insert :: (Eq a) => a -> c a -> c a
--     ...
instance Container IS.IntSet Int where
    insert      = IS.insert
    member      = IS.member
    union       = IS.union
    toList      = IS.toList
    fromList    = IS.fromList
    empty       = IS.empty
    singleton   = IS.singleton
