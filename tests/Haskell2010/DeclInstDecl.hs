{-# LANGUAGE Haskell2010, MultiParamTypeClasses #-}
module DeclInstDecl where

class Class0

class Class1 a where
  pat :: a
  fn :: a -> a

data Bool = True | False
data String
data Maybe a = Just a | Nothing

instance Class1 Bool
instance Class1 (Bool)
instance Class1 (a `Bool` b)
instance Class1 (Bool a b)

instance Class1 a => Class1 (Maybe a)

instance Class1 Bool where
  pat = True
  fn True = False
  fn False = True
