module SashoLib (
		 (.&&),
                 (.||),
		 (.*),
                 (...),

                 (<<),

                 (>>==),

        Stack (..),

        comp2_1,

        notp,
	takeMiddle,
	log2,

         integerize,
         integerize2,

	 ilog2,
         isqrt,
         divUp,
         subtr,

         hex,

         tr,

--         mkList,

         findInStack,
         maybeLookup,
         maybeMapAdjust,
         fromJustMsg,
         maybeApply,
         MaybeT, runMaybeT,
         maybeM,
                    
        modifyListHead,
        mapOnTail,

        applyWithDefault,

        splice,
        runSumFrom0,
        filterList,
        breakList,
        spanList,

        mapOne,

		 pair2,
		 pair3,

                 mapTuple2,

                 mapTupleM2,

                 projSnd,
                 projFst,

        tuple2list2,

        myLiftM,
        scanM,
        unfoldrM,
        replicateM,
        sumM,
        concatMapM,

        strictList,
        iterateList,

		 factorial,
                 choose,
                 sumOp,
                 mapInputs2,
                
                tup3_get1, tup3_get2, tup3_get3,
                tup3_proj1, tup3_proj2, tup3_proj3,

                tup4_get1, tup4_get2, tup4_get3, tup4_get4, 
                tup4_proj_1,  tup4_proj_2,  tup4_proj_3,  tup4_proj_4,

                tup5_get1

		)
    where

import List (isPrefixOf, union)

import Monad (MonadPlus, mzero, mplus, msum, liftM)

import Control.Monad.Error (Error, noMsg, ErrorT, runErrorT, MonadError(..))

import Control.Monad.Trans (MonadTrans, lift)

import Numeric                      (showHex)

import qualified Data.Map as Map




-- give a predicate which is an 'and' of two predicates
infixr 3 .&&
(.&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .&& g = \x -> f x && g x

infixr 4 .||
f .|| g = \x -> f x || g x

enumAll :: (Enum a) => [a]
enumAll = undefined



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


-- like >>= except the function is outside the monad, so this puts in a "return" for us
infixl 1  >>==
(>>==) :: (Monad m) => m a -> (a -> b) -> m b
k >>== f    = k >>= return . f


-- using the subtraction operator applied to one arg is tricky (needs a flip, and
-- something like (- x) is taken as (negate x)), hence:
subtr x = \y -> y - x


hex :: Integral a => a -> String
hex x = ("0x"++) . Numeric.showHex x $ ""

{-
instance Show String where
    -- try to deal with surrounding quotes
    show ('"' : chars)  = init chars
    show x              = ('"':x) ++ ['"']
-}


-- compose a function on 2 args with a function on one arg
-- same fixity as (.)
infixr 9 ...
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f ... g = \x y -> f (g x y)

f `comp2_1` g = \x y -> f (g x) (g y)


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


-- filter some sublist out of a list
filterList :: (Eq a) => [a] -> [a] -> [a]
filterList _   [] = []
filterList bad xs = let (pre,badL) = breakList (bad `isPrefixOf`) xs
                        postBad    = drop (length bad) badL
                    in pre ++ (filterList bad postBad)


-- iterate a function which produces a finite list, by re-applying it on each output, and
-- then concatenating
iterateList :: (a -> [a]) -> a -> [a]
iterateList f x = let fx = f x
                  in 
                    fx ++ concatMap (iterateList f) fx



-- a version of unfoldr for use in a Monad
unfoldrM :: (Monad m)  =>  ( b -> m (Maybe (a, b)) )  ->  b  ->  m [a]
unfoldrM f x  = do maybe_res <- f x
                   case maybe_res of
                                  (Just (y,x')) -> do rest <- unfoldrM f x'
                                                      return (y : rest)
                                  (Nothing)     -> return []

replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM = sequence ... replicate

{-
fooM :: (Monad m) => (a -> m b) -> [m a] -> m [b]
fooM f xs = 
-}

sumM ::  (Num a, Monad m) => [a] -> m a
sumM = myLiftM sum


concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = (liftM concat) . mapM f

-- substitute a sublist for another list
tr :: (Eq a) => ([a],[a]) -> [a] -> [a]
tr _         [] = []
tr (from,to) xs = let (pre,badL)        = breakList (from `isPrefixOf`) xs
                      postBad           = drop (length from) badL
                  in  if ((length pre) == (length xs)) -- 'from' was not found
                      then xs
                      else pre ++ to ++ (tr (from,to) postBad)



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



-- lift a function into a monad. The difference from liftM is that the result of myLiftM
-- takes a value not in the monad, so it's useful on the RHS of >>= (among others)
myLiftM :: (Monad m) => (a -> b) -> (a -> m b)
myLiftM f x = return (f x)


-- like scanl, but for monadic functions
scanM            :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]
scanM f a []     =  return [a]
scanM f a (x:xs) =  do y    <- f a x
                       rest <- scanM f y xs
                       return (a:rest)



-- force a Map.lookup in the Maybe monad
maybeLookup :: Ord k => k -> [Map.Map k a] -> Maybe a
maybeLookup key maps = findInStack (Map.lookup key) maps



-- a version of Map.adjust which tells if it changed anything or not (ie.
-- whether the key was found)
maybeMapAdjust :: (MonadPlus m, Ord k) => (a -> a) -> k -> Map.Map k a -> m (Map.Map k a)
maybeMapAdjust f k m    = if Map.member k m
                          then return $ Map.adjust f k m
                          else mzero

-- fromJust, with an error message in case of Nothing
fromJustMsg msg (Just x) = x
fromJustMsg msg Nothing  = error $ "fromJust Nothing: " << msg



-- this version of map is mostly the identity, but the first time that f returns
-- non-Nothing we'll actually use that result.
-- TODO: there must be a better way to do this with the Maybe monad
mapOne :: (a -> Maybe a) -> [a] -> [a]
mapOne f (x:xs) = case f x of
                           Nothing  -> x : mapOne f xs
                           Just x'  -> x': xs
mapOne _ [] = []


modifyListHead _ [] = error "modifyListHead on empty list!"
modifyListHead f (x:xs) = (f x : xs)

mapOnTail f []          = error "mapOnTail on empty list!"
mapOnTail f (x:xs)      = x : (map f xs)


{-
-- shortcut isn't quite working for now...

-- newtype MaybeT m a = ET (ErrorT () m a)
type MaybeT = ErrorT ()


runMaybeT :: (Monad m) => MaybeT m a -> m (Maybe a)
runMaybeT c = do let (ET c') = c
                 v <- runErrorT c'
                 return (either (const Nothing) Just v)
runMaybeT c = do v <- runErrorT c
                 return (either (const Nothing) Just v)
-}

-- not quite sure about this, but can be useful
instance MonadError () (Maybe) where
    throwError _            = Nothing
    Nothing `catchError` h  = h ()
    Just x  `catchError` _  = Just x


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

-- same for function on 2 params
integerize2 f x y = ceiling $ f (fromIntegral x) (fromIntegral y) 


ilog2 x 
    | x > 0     = integerize log2 x
    | otherwise = 0

isqrt = integerize sqrt


-- | given 2 functions 'f' and 'g', produce one function 'h' s.t. h x = (f x, g x)
pair2 :: (a -> b) -> (a -> c) -> (a -> (b,c))
pair2 f g = \x -> (f x, g x)

pair3 :: (a -> b) -> (a -> c) -> (a -> d) -> (a -> (b,c,d))
pair3 f g h = \x -> (f x, g x, h x)


-- | map a function over a pair, returning the resulting pair
mapTuple2 :: (a -> b) -> (a,a) -> (b,b)
mapTuple2 f (x,y) = (f x, f y)

-- | map a monadic function over a pair, returning the result pair in the Monad
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

fib             = 1 : 1 : [ a+b | (a,b) <- zip fib (tail fib) ]

-- simple encapsulation for a summation with limit 'a' to 'b' and
-- summed function 'f'
sumOp f a b = sum . map f $ [a..b]


runSumFrom0 :: (Num a) => [a] -> [a]
runSumFrom0 = init . scanl (+) 0


-- update a range (offset and length) of a list.
-- more precisely, replace the range (offset,len) with 'news' (regardless of its length)
splice (offset,len) news l = (take offset l) ++
                             news ++
                             (drop (offset + len) l)


-- make a list from a tuple
tuple2list2 (x,y) = [x,y]


-- have a list of functions on 2 params, run them over a fixed pair of
-- parameters
mapInputs2 :: a -> b -> [a -> b -> c] -> [c]
mapInputs2 x y fs = map (\f -> f x y) fs


-- ! Monad version of Prelude.maybe
maybeM :: (Monad m) => m b -> (a -> m b) -> Maybe a -> m b
maybeM def f x = case x of Just x' -> f x'
                           Nothing -> def


data OneOf3 a b c = Num1 a | Num2 b | Num3 c

oneof3 f1 f2 f3 x = case x of Num1 x -> f1 x
                              Num2 x -> f2 x
                              Num3 x -> f3 x



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
tup4_get1 (x1,x2,x3,x4) = x1
tup4_get2 (x1,x2,x3,x4) = x2
tup4_get3 (x1,x2,x3,x4) = x3
tup4_get4 (x1,x2,x3,x4) = x4

tup5_get1 (x1,x2,x3,x4,x5)  = x1
tup5_get2 (x1,x2,x3,x4,x5)  = x2
tup5_get3 (x1,x2,x3,x4,x5)  = x3
tup5_get4 (x1,x2,x3,x4,x5)  = x4
tup5_get5 (x1,x2,x3,x4,x5)  = x5

tup4_proj_1 f (x1,x2,x3,x4) = (f x1, x2  , x3  , x4  )
tup4_proj_2 f (x1,x2,x3,x4) = (x1  , f x2, x3  , x4  )
tup4_proj_3 f (x1,x2,x3,x4) = (x1  , x2  , f x3, x4  )
tup4_proj_4 f (x1,x2,x3,x4) = (x1  , x2  , x3  , f x4)

