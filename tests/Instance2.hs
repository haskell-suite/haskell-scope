{-# LANGUAGE ExplicitForAll #-}
module Instance2 where

data String
data List a

class Show a where
  show :: a -> String

instance Show a => Show (List a)

instance forall a. Show a => Show (List a)
