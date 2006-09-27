module Container where


import List (union)
import qualified Data.IntSet                    as IS


-- a set class, without mapping
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


instance Container IS.IntSet Int where
    insert      = IS.insert
    member      = IS.member
    union       = IS.union
    toList      = IS.toList
    fromList    = IS.fromList
    empty       = IS.empty
    singleton   = IS.singleton
