module Mapping where


import qualified Data.Map               as Map


-- the type of the container determines the type of the key and value, hence functional
-- dependency c -> k a
class (Ord k) => Mapping c k a | c -> k a where
    insert      :: k -> a -> c  -> c
    lookup      :: (Monad m) => k -> c -> m a
    empty       :: c
    toList      :: c -> [(k,a)]

-- use a list as a mapping.
instance (Ord k) => Mapping ([(k,a)]) k a where
    insert k a l    = (k,a):l
    lookup k l      = let m_v = Prelude.lookup k l
                      in        -- convert from Maybe to this Monad
                        maybe (fail "")
                              (\v -> return v)
                              m_v
    empty           = []
    toList          = id

-- a Map makes a very good Mapping of course
instance (Ord k) => Mapping (Map.Map k a) k a where
    insert      = Map.insert
    lookup      = Map.lookup
    empty       = Map.empty
    toList      = Map.toList


-- | Return the keys of a Mapping
keys :: (Mapping c k a) => c -> [k]
keys = map fst . toList
