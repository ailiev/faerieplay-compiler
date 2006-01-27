module Container where


import List (union)

-- a set class, without mapping
class (Eq a) => Container c a where
    insert      :: a -> c a -> c a
    member      :: a -> c a -> Bool
    union       :: c a -> c a -> c a
    toList      :: c a -> [a]
    fromList    :: [a] -> c a
    empty       :: c a
    singleton   :: a -> c a


instance (Eq a) => Container [] a where
    insert      = (:)
    member      = elem
    union       = List.union
    toList      = id
    fromList    = id
    empty       = []
    singleton x = [x]
