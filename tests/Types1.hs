{-# LANGUAGE ExplicitForAll #-}
module Types1 where

x :: a
x = x

y :: forall b. b
y = y

z :: Show c => c
z = z

f :: a -> b -> a
f = f

g :: a -> forall a. a
g = g
