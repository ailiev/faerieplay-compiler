{-# OPTIONS_GHC -cpp #-}

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


module Faerieplay.Common where

import Control.Monad.Error (MonadError(..),Error(strMsg),throwError,noMsg)

import qualified Debug.Trace                    as Trace

import IlievUtils.Misc                                 (StreamShow (..))

import Faerieplay.ErrorWithContext


-- This is the type of our type error representation.
data MyError = Err {line::Int, reason::String} deriving (Show)

type MyErrorCtx = ErrorWithContext MyError String


-- throw with a blank initial context
throwErrorCtx e = throwError $ EWC (e,[])


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

type ErrCtxMonad = Either MyErrorCtx

-- | Runtime flags, built from the command line or other configuration settings, and
-- passed to the various parts in different ways, eg. as part of MyState in CircGen.hs.
data RunFlag = DumpGates | DumpGraph | DoPrint | Verbose | RunTraceBinary
    deriving (Read,Show,Eq,Ord)


-- | assertion at the compiler level, ie. a failure indicates a compiler bug.
-- in an arbitary Error monad
compilerAssert :: (Error e, MonadError e m) => Bool -> String -> m ()
compilerAssert pred msg
    | pred      = return ()
    | otherwise = throwCompilerErr msg


-- | Raise a compiler exception within the current Error monad.
throwCompilerErr :: (Error e, MonadError e m) => String -> m a
throwCompilerErr msg = throwError $ strMsg ("Compiler internal bug: " ++ msg)


#ifdef TRACE
-- reversed order is much better!
trace = flip Trace.trace
#else
trace = const -- flip Trace.trace
#endif

data LogPrio = DUMP | DEBUG | INFO | PROGRESS | WARNING | ERROR
   deriving (Show,Eq,Ord)


cMINPRIO = 
#ifdef TRACE
--       DEBUG
       DUMP
#else
       PROGRESS
#endif


logmsg prio x msg = if prio >= cMINPRIO
                    then Trace.trace (show prio ++ ": " ++ msg) x
                    else x

logProgress = logmsg PROGRESS
logDebug    = logmsg DEBUG
logDump     = logmsg DUMP

infix 0 `trace`
