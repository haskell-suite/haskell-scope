module Instance4 where

data String

class Show a where
  show :: a -> String

notMember = notMember

instance Show String where
  notMember x = show x
