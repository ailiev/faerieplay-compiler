{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}

module SashoLib (
		 (.&&),
                 (.||),
		 (.*),
                 (...),

                 StreamShow (..),
                 (<<),

                 (>>==),


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
         trs,

--         mkList,

         maybeMapAdjust,
         fromJustMsg,
         maybeApply,
         MaybeT, runMaybeT,
                    
        modifyListHead,
        mapOnTail,
        interleave,

        iterateTree,
        pruneTree,

        nubOrds,

        compareWith,

        applyWithDefault,

        strictEval,

        splice,
        runSumFrom0,
        filterList,
        breakList,
        spanList,

        mapOne,
--        mapDupsBy,
        mapAccumDupsBy,

		 pair2,
		 pair3,

                 mapTuple2,

                 mapTupleM2,

                 projSnd,
                 projFst,

        tuple2list2,

        myLiftM,
        liftArgM,

        scanM,
        unfoldrM,
        replicateM,
        repeatM,
        sumM,
        concatMapM,
        zipWith3M,

        strictList,
        iterateList,

        bitMask,
        getBits,

        mapTreeM, mapTree,

		 factorial,
                 choose,
                 sumOp,
                 mapInputs2,

                 proj_tup2,
                
                tup3_get1, tup3_get2, tup3_get3,
                tup3_proj1, tup3_proj2, tup3_proj3,

                tup4_get1, tup4_get2, tup4_get3, tup4_get4, 
                tup4_proj_1,  tup4_proj_2,  tup4_proj_3,  tup4_proj_4,

                tup5_get1,

        DocAble(..),
        expand


		)
    where

import List (isPrefixOf, union, intersperse, mapAccumL, partition)
import qualified List

import Monad (MonadPlus, mzero, mplus, msum, liftM)

import Control.Monad.Error (Error, noMsg, ErrorT, runErrorT, MonadError(..))
import Control.Monad.Identity (runIdentity)

import qualified    Text.PrettyPrint            as PP

import Control.Monad.Trans (MonadTrans, lift)

import Numeric                      (showHex)

import Data.Bits            ((.&.), (.|.))
import qualified Data.Bits                      as Bits

import qualified Data.Tree                      as Tree
import qualified Data.Map                       as Map





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

-- first a cousin of the Show class specially for this purpose
class StreamShow a where
    strShows :: a -> ShowS
    strShow  :: a -> String
    -- and default definitions
    strShows x = ((strShow x) ++)
    strShow  x = strShows x ""




instance StreamShow String where
    strShows s = (s ++)
    strShow  s = s

instance StreamShow Int     where strShows = showsPrec 0
instance StreamShow Integer where strShows = showsPrec 0

strShowsList :: (StreamShow a) => [a] -> ShowS
strShowsList = (foldl (.) id) . intersperse (", " ++) . map strShows

instance (StreamShow a) => StreamShow [a] where
    strShows = strShowsList

instance (StreamShow a, StreamShow b) => StreamShow (a,b) where
    strShows (x,y) =    strShows "(" .
                        strShows x . strShows ", " . strShows y .
                        strShows ")"


(<<) :: (StreamShow a, StreamShow b) => (a -> b -> String)
x << y = strShow x ++ strShow y
--    where cleanup = (filter ((/= '"')))




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


-- | compose a function on 2 args with a function on one arg
-- same fixity as (.)
infixr 9 ...
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f ... g = \x y -> f (g x y)


-- | compose a function g on one arg with a function f on 2 args, applying g to two inputs
-- before passing to f
infixr 9 `comp2_1`
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


-- | iterate a function which produces a finite list, by re-applying it on each output, and
-- then concatenating
iterateList :: (a -> [a]) -> a -> [a]
iterateList f x = let fx = f x
                  in 
                    fx ++ concatMap (iterateList f) fx


-- remove duplicates in a list of Ord instance (ie. can be sorted); should be
-- much more efficient than List.nub, which is O(n^2)
nubOrds :: (Ord a) => [a] -> [a]
nubOrds = map head . List.group . List.sort

{-
-- set difference for Ord instances, using a Map
diffOrds :: (Ord a) => [a] -> [a] -> [a]
diffOrds x y    = let [x', y'] = map sort [x, y]
 -}                    


compareWith f x y = compare (f x) (f y)


-- apply a function to all second and further instances of an item in a list, with a given
-- equality predicate
{-
mapDupsBy :: (a -> a -> Bool) -> (a -> a) -> [a] -> [a]
mapDupsBy eq f xs = let (ys, _) = foldl g ([],[]) xs
                    in
                      reverse ys
    where g (ys, uniqs) x = let (y, uniqs') = if any (eq x) uniqs -- have already seen x
                                              then (f x, uniqs)
                                              else (x  , x:uniqs)
                            in
                              (y:ys, -- this is efficient, but will build the list in
                                     -- reverse
                               uniqs')
-}


-- | mapAccum a function on equivalent values in a list, presumably to make them
-- different via numbering or such. 'f' will be applied to the first instance of every
-- value too, with 'start' as the accumulator value.
mapAccumDupsBy :: (a -> a -> Bool)
               -> (acc_t -> a -> (acc_t,b))
               -> acc_t
               -> [a]
               -> [b]
mapAccumDupsBy eq f start xs = snd $ mapAccumL g [] xs
-- the accumulator is an assoc list of type (a, acc_t)
    where -- g :: [(a, acc_t)] -> a -> ( [(a, acc_t)], a )
          g accums x = let (mb_accum, rest) = extractOne (eq x . fst) accums
                           (acc_val', y)    = maybe (f start x)
                                                    (\acc -> f (snd acc) x)
                                                    mb_accum
                           in
                             ((x,acc_val') : rest, -- new accumulator
                              y -- new list element
                             )


extractOne   p  xs = let (matches, rest) = partition p xs
                     in
                       case matches of
                         []            -> (Nothing, xs)
                         [m]           -> (Just m, rest)
                         (m1:m_rest)   -> (Just m1, m_rest++rest)


-- a version of unfoldr for use in a Monad
unfoldrM :: (Monad m)  =>  ( b -> m (Maybe (a, b)) )  ->  b  ->  m [a]
unfoldrM f x  = do maybe_res <- f x
                   case maybe_res of
                                  (Just (y,x')) -> do rest <- unfoldrM f x'
                                                      return (y : rest)
                                  (Nothing)     -> return []

replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM = sequence ... replicate

-- | repeat a monadic action to infinity.
repeatM :: (Monad m) => m a -> m [a]
repeatM = sequence . repeat

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

-- | substitute each of several patterns.
trs :: (Eq a) => [([a],[a])] -> [a] -> [a]
trs subs = foldl (.) id $ reverse $ map tr subs


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


-- | force a strict evaluation of a value, returning it
strictEval x = x `seq` x



maybeApply :: Maybe (a -> a) -> a -> a
maybeApply (Just f) x = f x
maybeApply Nothing  x = x



-- integer division with rounding *up*
infixl 7 `divUp`
divUp :: (Integral a) => a -> a -> a
divUp x y = let (d,m) = divMod x y
            in  d + (signum m)




-- REMINDER: we have in Control.Monad.Error:
-- instance (Error e) => MonadPlus (Either e) where ...



-- lift a function into a monad. The difference from liftM is that the result of myLiftM
-- takes a value not in the monad, so it's useful on the RHS of >>= (among others)
myLiftM :: (Monad m) => (a -> b) -> (a -> m b)
myLiftM f x = return (f x)

-- | lift a function which injects into a Monad into one which has input and result in the
-- Monad. Have used it on mapM.
liftArgM :: (Monad m) => (a -> m b) -> (m a -> m b)
liftArgM f m_x = do x <- m_x
                    f x


-- like scanl, but for monadic functions
scanM            :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]
scanM f a []     =  return [a]
scanM f a (x:xs) =  do y    <- f a x
                       rest <- scanM f y xs
                       return (a:rest)

zipWith3M f xs ys zs = sequence $ zipWith3 f xs ys zs



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

interleave [] []            = []
interleave (x:xs) (y:ys)    = (x:y:interleave xs ys)



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


-- not quite sure about this, but can be useful
instance MonadError () [] where
    throwError _            = []
    [] `catchError` h  = h ()
    xs  `catchError` _  = xs


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

-- | turn a pair of functions to a function on pairs
proj_tup2 (f,g) (x,y) = (f x, g y)

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




--
-- some bit manipulation routines.
--

-- | Get the value of a range of bits (inclusive) in an instance of Bits.
getBits :: Bits.Bits i => i -> (Int,Int) -> i
i `getBits` (a,b) = ( i .&. (bitMask (a,b)) ) `Bits.shiftR` a

-- | Get a bitmask for a range of bits (inclusive)
bitMask :: Bits.Bits i => (Int,Int) -> i
bitMask (i,j)     = ( Bits.complement ((Bits.complement 0) `Bits.shiftL` (j-i+1)) )  `Bits.shiftL` i




-- make a list from a tuple
tuple2list2 (x,y) = [x,y]


-- have a list of functions on 2 params, run them over a fixed pair of
-- parameters
mapInputs2 :: a -> b -> [a -> b -> c] -> [c]
mapInputs2 x y fs = map (\f -> f x y) fs



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



----------------------
-- Tree functions
----------------------

-- | map a monadic function on all the nodes of a 'Tree'.
mapTreeM :: (Monad m) => (a -> m b) -> Tree.Tree a -> m (Tree.Tree b)
mapTreeM f t@(Tree.Node l ts) = do l'  <- f l
                                   ts' <- mapM (mapTreeM f) ts -- works for ts == []
                                   return $ Tree.Node l' ts'

-- | map a normal function over a tree
mapTree :: (a -> b) -> Tree.Tree a -> Tree.Tree b
mapTree f t = runIdentity $ mapTreeM (myLiftM f) t


-- | iterate a function which produces a finite list, to produce a Tree
iterateTree :: (a -> [a]) -> a -> Tree.Tree a
iterateTree f x = Tree.Node x (map (iterateTree f) (f x))


-- | prune a tree up to and including depth 'd' (ie. the root is always kept)
pruneTree :: (Ord a, Num a) => a -> Tree.Tree b -> Tree.Tree b
pruneTree d (Tree.Node r subs)
    | d <= (fromInteger 0)  = Tree.Node r []
    | otherwise             = Tree.Node r (map (pruneTree (d-1)) subs)


-- | class of types which are convertable to a Doc for pretty-printing.
class DocAble a where
    doc     :: a -> PP.Doc



-- | Take an assoc list of (key, values), and expand it to a (longer) list of
-- (key,value), where each 'values' has been expanded to one value per element.
expand :: [(a, [b])] -> [(a,b)]
-- use foldr to avoid quadratic blowup with (++)
expand xs = foldr f [] xs
    where f (a,bs) dones = [(a,b) | b <- bs] ++ dones
