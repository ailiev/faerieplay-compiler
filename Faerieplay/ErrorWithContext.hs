{-# OPTIONS_GHC -fglasgow-exts #-}
-- -fglasgow-exts: for multi-param type classes, in MonadError

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


-- ErrorWithContext.hs
-- by Alex Iliev
-- an extended Error monad, with support for gathering context information as it "unwinds"
-- the call stack.

module Faerieplay.ErrorWithContext where


import Control.Monad.Error                  (Error(..),ErrorT(..))

-- | class of types suitable for an error context. It's just a marker class, with no
-- operations defined.
class (Show c) => ErrorContext c

-- | A string is a good simple context.
instance ErrorContext String


-- | an Error with Context, which is just a list of ErrorContext instances.
-- was too complicated to make a class out of this.
newtype (Error e, ErrorContext c) =>
    ErrorWithContext e c = EWC (e, [c])
    deriving (Show)


-- | An ErrorWithContext is an Error.
instance (Error e, ErrorContext c) => Error (ErrorWithContext e c) where
    noMsg       = EWC (noMsg, [])
    strMsg m    = EWC ((strMsg m), [])


{-
-- class (MonadError e m, ErrorContext c) => MonadErrorWithContext e c m | m -> c e where
-- this instance is the same as for
-- Management of contexts is done only in the <?> operator.
-- instance (Error e) => Monad (Either e)
-- in Control.Monad.Error
instance (Error e, ErrorContext c) => Monad (ErrorWithContext e c) where
    return r                        = EWC (Right r) []
    err@(EWC (Left  l) cs)  >>= _   = EWC (Left  l) cs
    (EWC (Right r) [])      >>= g   = g r


instance (Error e, ErrorContext c) => MonadError (e,c) (ErrorWithContext e c) where
    -- start an error with an empty context
    throwError e                                = EWC (Left e) []
    err@(EWC (Left  _) ctxs)    `catchError` h  = h (err, ctxs)
    -- should not have any contexts in a non-error value
    r@  (EWC (Right _) [])      `catchError` _  = r

-}

    
-- | A context annotation operator similar to the same one in Parsec
-- operates on an ErrorT with any enclosed Monad.
(<?>) :: (Error e, ErrorContext c, Monad m) =>
         ErrorT (ErrorWithContext e c) m a -> c -> ErrorT (ErrorWithContext e c) m a
x <?> ctx       = ErrorT $
                  -- this 'do' is in the ErrorT's inner monad, 'm' in the type signature
                  -- above.
                  do x_err <- runErrorT x
                     case x_err of (Left (EWC (e, cs)))      -> return $ Left $ EWC (e, (ctx:cs))
                                   r@(Right _)               -> return r

setContext ctx x = x <?> ctx

-- withIdContext :: (ErrorWithContext e c m) => (a -> m b) -> (a -> m b)
withIdContext f x = f x <?> x
