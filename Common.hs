{-# OPTIONS_GHC -cpp #-}

module Common where

import Control.Monad.Error (Error, noMsg, strMsg)
import qualified Data.Map                       as Map

import qualified Debug.Trace                    as Trace

import SashoLib                                 (StreamShow (..))


-- This is the type of our type error representation.
data MyError = Err {line::Int, reason::String} deriving (Show)

-- We make it an instance of the Error class
instance Error MyError where
  noMsg    = Err 0 "Type error"
  strMsg s = Err 0 s

instance StreamShow MyError where
    strShows  (Err line reason) = ((show line) ++) . (": " ++) . (reason ++)



-- For our monad type constructor, we use Either TypeError
-- which represents failure using Left TypeError, or a
-- successful result of type a using Right a.
-- note that we now have a type "ErrMonad a", synonym for "Either TypeError a"
type ErrMonad = Either MyError

-- | Runtime flags, built from the command line or other configuration settings, and
-- passed to the various parts in different ways, eg. as part of MyState in CircGen.hs.
data RunFlags = DoPrint
    deriving (Read,Show,Eq,Ord)



#ifdef TRACE
-- reversed order is much better!
trace = flip Trace.trace
#else
trace = const -- flip Trace.trace
#endif

data LogPrio = DEBUG | INFO | PROGRESS | WARNING | ERROR
   deriving (Show,Eq,Ord)


cMINPRIO = 
#ifdef TRACE
       DEBUG
#else
       PROGRESS
#endif


logmsg prio x msg = if prio >= cMINPRIO
                    then Trace.trace (show prio ++ ": " ++ msg) x
                    else x

logProgress = logmsg PROGRESS
logDebug    = logmsg DEBUG

infix 0 `trace`

