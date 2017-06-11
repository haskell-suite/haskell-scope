{-# LANGUAGE Haskell2010 #-}
module DeclPatBind where

data Bool = True | False
data Maybe a = Just a | Nothing

True = False

Just a = a

Just b = c
  where c = b

Just (Just (Just Nothing)) = Nothing

d `Nothing` e = ()

True
  | a = True
  | Nothing = True
  | True = False
