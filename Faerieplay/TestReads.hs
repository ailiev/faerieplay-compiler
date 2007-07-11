module Faerieplay.TestReads where

data D = D Int Char
       deriving (Show)

instance Read D where
    readsPrec _ s = do (i,s1)   <- reads s
                       (',',s2) <- reads s1
                       (c,s3)   <- reads s2
                       return $ (D i c, s3)
