module Mapping where


-- the type of the container determines the type of the key and value, hence functional
-- dependency c -> k a
class (Ord k) => Mapping c k a | c -> k a where
    insert      :: k -> a -> c  -> c
    lookup      :: (Monad m) => k -> c -> m a
    empty       :: c



instance (Ord k) => Mapping ([(k,a)]) k a where
    insert k a l    = (k,a):l
    lookup k l      = let m_v = Prelude.lookup k l
                      in        -- convert from Maybe to this Monad
                        maybe (fail "")
                              (\v -> return v)
                              m_v
    empty           = []
