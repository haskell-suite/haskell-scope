module Instance1 where

data String

class Show a where
  show :: a -> String

instance Show String where
  show x = show x
