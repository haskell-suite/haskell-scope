{-# LANGUAGE ExplicitForAll #-}
module Types2 where

class Show a

x :: a
x = x

y :: forall a. a
y = y

z :: Show a => a
z = z

q :: forall a. Show a => a
q = q
