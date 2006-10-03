module Stack where


import Monad                        (MonadPlus, msum)
import qualified Data.Map           as Map


-- | A class of types with Stack-like functionality. Not sure if anything other than a
-- list will be used here.
-- Require that the types used are instances of 'Functor'
class (Functor s) => Stack s where
    -- | Return the top element
    peek :: s a -> a
    -- | pop the stack, does not return popped element
    pop  :: s a -> s a
    push :: a -> s a -> s a
    -- | modify the top element with a function, returning the new stack
    modtop :: (a -> a) -> s a -> s a
    -- | convert to a list, keeping the list order same as the stack order.
    toList :: s a -> [a]


instance Stack [] where
    peek    = head
    pop     = tail
    push x s = (x:s)
    modtop f (x:xs) = f x : xs
    toList  = id




-- apply an operation successively down a list, until it does not fail, then
-- return that result
-- (which is what msum does, provided that "fail" is mzero)
findInStack :: (MonadPlus m, Stack s) => (a -> m b) -> s a -> m b
findInStack f stack = msum $ toList $ fmap f stack


-- | Try to look up a value in a stack of maps, starting from the top (head).
-- FIXME: rename to lookupInStack
maybeLookup :: Ord k => k -> [Map.Map k a] -> Maybe a
maybeLookup key maps = findInStack (Map.lookup key) maps
