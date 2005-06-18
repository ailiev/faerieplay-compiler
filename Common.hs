module Common where

import Control.Monad.Error (Error, noMsg, strMsg)




-- This is the type of our type error representation.
data MyError = Err {line::Int, reason::String} deriving (Show)



-- We make it an instance of the Error class
instance Error MyError where
  noMsg    = Err 0 "Type error"
  strMsg s = Err 0 s


-- For our monad type constructor, we use Either TypeError
-- which represents failure using Left TypeError, or a
-- successful result of type a using Right a.
-- note that we now have a type "ErrMonad a", synonym for "Either TypeError a"
type ErrMonad = Either MyError