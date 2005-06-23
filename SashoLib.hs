module SashoLib (
		 (.&&),
		 (.*),
                 (...),
         (<<),

        Stack (..),

        notp,
	takeMiddle,
	log2,

         integerize,
		 ilog2,
         isqrt,
         divUp,

         findInStack,
         maybeLookup,
         maybeMapAdjust,
         maybeApply,
         MaybeT, runMaybeT,
                    
        modifyListHead,
        applyWithDefault,

        mapOne,

		 pair2,
		 pair3,

                 mapTuple2,

                 mapTupleM2,

                 projSnd,
                 projFst,

        myLiftM,
        unfoldrM,
        replicateM,
        sumM,
        concatMapM,

        strictList,

		 factorial,
                 choose,
                 sumOp,
                 mapInputs2,
                
                tup3_get1, tup3_get2, tup3_get3,
                tup3_proj1, tup3_proj2, tup3_proj3,

                tup4_proj_1,  tup4_proj_2,  tup4_proj_3,  tup4_proj_4
		)
    where

import List (isPrefixOf, union)

import Monad (MonadPlus, mzero, mplus, msum, liftM)

import Control.Monad.Error (Error, noMsg, ErrorT, runErrorT)

import Control.Monad.Trans (MonadTrans, lift)

import qualified Data.Map as Map



-- give a predicate which is an 'and' of two predicates
infixr 3 .&&
(.&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .&& g = \x -> f x && g x


-- instance (Eq a) => Eq (a -> a) where
--      f == g = (\x -> (f x) == (g x))
--      f /= g = (\x -> (f x) /= (g x))

-- instance (Num a) => Num (a -> a) where
--     f + g = \a b -> f a + g b
--     f - g = \a b -> f a - g b
--     f * g = \a b -> f a * g b
--     negate f = negate . f
--     abs f    = abs . f
--     signum f = signum . f
--     fromInteger f = fromInteger . f


-- join two arithmetic operations with a multiply
infixl 7 .*
(.*) :: (Num a) => (a -> a) -> (a -> a) -> (a -> a)
f .* g = \x -> f x * g x



-- return a predicate which is the 'not' of another predicate
notp :: (a -> Bool) -> (a -> Bool)
notp f = not . f


-- operator to build strings
(<<) :: (Show a, Show b) => (a -> b -> String)
x << y = cleanup $ (show x) ++ (show y)
    where cleanup = (filter ((/= '"')))


-- compose a function on 2 args with a function on one arg
-- same fixity as (.)
infixr 9 ...
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f ... g = \x y -> f (g x y)


{-
takeWhileList :: ([a] -> Bool) -> [a] -> [a]
takeWhileList p [] = []
takeWhileList p l@(x:xs) =
    | p l = x : takeWhileList p xs
    | otherwise = []

dropWhileList p [] = []
dropWhileList p xs@(x:xs')
    | p xs      =  dropWhileList p xs'
    | otherwise =  xs
-}

spanList p []            = ([],[])
spanList p xs@(x:xs') 
            | p xs      =  (x:ys,zs) 
            | otherwise =  ([],xs)
    where (ys,zs) = spanList p xs'

breakList p = spanList (not . p)


filterList :: (Eq a) => [a] -> [a] -> [a]
filterList _   [] = []
filterList bad xs = let (pre,badL) = breakList (bad `isPrefixOf`) xs
                        postBad    = drop (length bad) badL
                    in pre ++ (filterList bad postBad)


-- a version of unfoldr for use in a Monad
unfoldrM :: (Monad m)  =>  ( b -> m (Maybe (a, b)) )  ->  b  ->  m [a]
unfoldrM f x  = do maybe_res <- f x
                   case maybe_res of
                                  (Just (y,x')) -> do rest <- unfoldrM f x'
                                                      return (y : rest)
                                  (Nothing)     -> return []

replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM = sequence ... replicate


sumM ::  (Num a, Monad m) => [a] -> m a
sumM = myLiftM sum


concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = (liftM concat) . mapM f



-- from http://haskell.org/hawiki/LicensedPreludeExts
-- Evaluates every element of the list when any part of the list is examined.
-- Someone modified (improved) this on the unlicensed PreludeExts page.
strictList :: [a] -> [a]
strictList ls@(a : more) = seq a $ seq (strictList more) $ ls
strictList ls = ls


-- apply f to (Maybe x), using def if x is Nothing
applyWithDefault :: (a -> a) -> a -> Maybe a -> a
applyWithDefault f def x = case x of
                               Just x'  -> f x'
                               Nothing  -> def


maybeApply :: Maybe (a -> a) -> a -> a
maybeApply (Just f) x = f x
maybeApply Nothing  x = x



-- NOTE: needs multi-parameter type classes, which is not Haskell 98, but should
-- be in the next spec apparently
class Stack s where
    peek :: s a -> a
    pop  :: s a -> s a
    push :: a -> s a -> s a
    modtop :: (a -> a) -> s a -> s a


instance Stack [] where
    peek = head
    pop  = tail
    push x s = (x:s)
    modtop = modifyListHead


-- integer division with rounding *up*
infixl 7 `divUp`
divUp :: (Integral a) => a -> a -> a
divUp x y = let (d,m) = divMod x y
            in  d + (signum m)



-- apply an operation successively down a list, until it does not fail, then
-- return that result
-- (which is what msum does, provided that "fail" is mzero)
findInStack :: (MonadPlus m) => (a -> m b) -> [a] -> m b
findInStack f stack = msum (map f stack)



-- REMINDER: we have in Control.Monad.Error:
-- instance (Error e) => MonadPlus (Either e) where ...



-- lift a function into a monad. havent quite nailed why this cannot
-- be done my liftM, but they are clearly different
myLiftM :: (Monad m) => (a -> b) -> a -> m b
myLiftM f = \x -> return (f x)


-- force a Map.lookup in the Maybe monad
maybeLookup :: Ord k => k -> [Map.Map k a] -> Maybe a
maybeLookup key maps = findInStack (Map.lookup key) maps



-- a version of Map.adjust which tells if it changed anything or not (ie.
-- whether the key was found)
maybeMapAdjust :: (MonadPlus m, Ord k) => (a -> a) -> k -> Map.Map k a -> m (Map.Map k a)
maybeMapAdjust f k m    = if Map.member k m
                          then return $ Map.adjust f k m
                          else mzero

-- this version of map is mostly the identity, but the first time that f returns
-- non-Nothing we'll actually use that result.
-- TODO: there must be a better way to do this with the Maybe monad
mapOne :: (a -> Maybe a) -> [a] -> [a]
mapOne f (x:xs)
    | Nothing <- f_x    = x : mapOne f xs
    | Just x' <- f_x    = x' : xs
    where f_x = f x
mapOne _ [] = []


modifyListHead _ [] = error "modifyListHead on empty list!"
modifyListHead f (x:xs) = (f x : xs)


{-
-- shortcut isn't quite working for now...

newtype MaybeT = ErrorT ()

runMaybeT c = do v <- runErrorT c
                 return (either Just (const Nothing) v)

-}

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MaybeT m) where
    return a    = MaybeT $ return (Just a)
    m >>= k     = MaybeT $ do a <- runMaybeT m
                              case a of
                                     Nothing -> return Nothing
                                     Just x  -> runMaybeT (k x)
    fail _      = MaybeT $ return Nothing


instance (Monad m) => MonadPlus (MaybeT m) where
    mzero       = MaybeT $ return Nothing
    m `mplus` n = MaybeT $ do a <- runMaybeT m
                              case a of
                                     Nothing    -> runMaybeT n
                                     Just x     -> return (Just x)

instance MonadTrans MaybeT where
    lift m      = MaybeT $ do a <- m
                              return $ Just a



{-
class (Show a) => Appendable a where
    (<<)   :: String -> a -> String
    s << x = s ++ (show x)

instance Appendable String where
    (<<) = (++)

instance (Show a) => Appendable a where
    s << x = s ++ (show x)
-}

-- if a list has predicate results: [F, F, F, T, T, ..., T, F, F,...],
-- return the middle part that is True
takeMiddle :: (a -> Bool) -> [a] -> [a]
takeMiddle p = (takeWhile p) . (dropWhile (not . p))

log2 :: (Floating a) => a -> a
log2 = logBase 2


-- wrap a numeric function so it takes and returns an Integral type (with rounding)
-- integerize :: (Num a, Num b, Integral c, Integral d) => (a -> b) -> (c -> d)
integerize f = ceiling . f . fromIntegral


ilog2 x 
    | x > 0     = integerize log2 x
    | otherwise = 0

isqrt = integerize sqrt


-- given 2 functions f and g, produce one func h s.t. h x = (f x, g x)
pair2 :: (a -> b) -> (a -> c) -> (a -> (b,c))
pair2 f g = \x -> (f x, g x)

pair3 :: (a -> b) -> (a -> c) -> (a -> d) -> (a -> (b,c,d))
pair3 f g h = \x -> (f x, g x, h x)

mapTuple2 :: (a -> b) -> (a,a) -> (b,b)
mapTuple2 f (x,y) = (f x, f y)

mapTupleM2 :: (Monad m) => (a -> m b) -> (a,a) -> m (b,b)
mapTupleM2 f (x,y) = do x' <- f x
                        y' <- f y
                        return (x', y')


-- project a function onto members of a pair
projFst f (x,y) = (f x, y  )
projSnd f (x,y) = (x  , f y)



factorial :: Integral a => a -> a
factorial n = product [1..n]

-- using quot operator to make sure we get an integral result (which
-- should always be the case anyhow)
choose n k = factorial n `quot` (factorial k * factorial (n-k))


-- simple encapsulation for a summation with limit 'a' to 'b' and
-- summed function 'f'
sumOp f a b = sum . map f $ [a..b]



-- have a list of functions on 2 params, run them over a fixed pair of
-- parameters
mapInputs2 :: a -> b -> [a -> b -> c] -> [c]
mapInputs2 x y fs = map (\f -> f x y) fs


-- just a reminder, apply3 takes a function f and 3 params, and
-- applies f on the params
apply3 = ((($).).)


-- a version of scanl that throws away its last element
--scanl

-- helpers for 3-tuples!
tup3_get1 (x,_,_) = x
tup3_get2 (_,x,_) = x
tup3_get3 (_,_,x) = x

tup3_proj1 f (x,y,z) = (f x , y   , z  )
tup3_proj2 f (x,y,z) = (x   , f y , z  )
tup3_proj3 f (x,y,z) = (x   , y   , f z)


-- and for 4-tuples!
tup4_get i (x1,x2,x3,x4) = case i of
                                  1 -> x1
                                  2 -> x2
                                  3 -> x3
                                  4 -> x4
                                  _ -> error $ "No element " << i
                                               << "in a 4-tuple"

tup4_proj_1 f (x1,x2,x3,x4) = (f x1, x2  , x3  , x4  )
tup4_proj_2 f (x1,x2,x3,x4) = (x1  , f x2, x3  , x4  )
tup4_proj_3 f (x1,x2,x3,x4) = (x1  , x2  , f x3, x4  )
tup4_proj_4 f (x1,x2,x3,x4) = (x1  , x2  , x3  , f x4)

