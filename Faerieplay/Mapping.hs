{-# OPTIONS_GHC -fglasgow-exts #-}
-- -fglasgow-exts: for multi-param type classes

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


module Faerieplay.Mapping where


import qualified Data.Map               as Map
import qualified Data.IntMap            as IM


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
                        maybe (fail "Key not found in list Mapping")
                              (\v -> return v)
                              m_v
    empty           = []
    toList          = id

-- a Map makes a very good Mapping of course
-- with the slight detail that Map.lookup returns a Maybe, whereas we
-- want a generic Monad.
instance (Ord k) => Mapping (Map.Map k a) k a where
    insert      = Map.insert
    lookup k m  = -- convert from Maybe to this Monad
                  maybe (fail "Key not found in Map Mapping")
                        (\v -> return v)
                        (Map.lookup k m)
    empty       = Map.empty
    toList      = Map.toList


-- | A Mapping of Int to values, using IntMap
instance Mapping (IM.IntMap a) Int a where
    insert      = IM.insert
    -- standard library BUG: IntMap.lookup is not in any Monad but in Maybe
    -- this is in whatever Monad the user wants.
    lookup k m  = do let mb_val = IM.lookup k m
                     case mb_val of Nothing     -> fail "Key not found in IntMap Mapping"
                                    Just val    -> return val
    empty       = IM.empty
    toList      = IM.toList



-- | Return the keys of a Mapping
keys :: (Mapping c k a) => c -> [k]
keys = map fst . toList

-- | Do a series of updates to the map
insertMany :: (Mapping c k a) => [(k,a)] -> c -> c
insertMany news m = foldr (\(k,v) m -> insert k v m) m news
