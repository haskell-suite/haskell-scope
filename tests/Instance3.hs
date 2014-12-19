module Instance3 where

data String

class Show a where
  show :: a -> String

instance String `Show` String
