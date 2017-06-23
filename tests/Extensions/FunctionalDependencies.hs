{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE FunctionalDependencies #-}
module FunctionalDependencies where

class FunDep1 a b | a -> b

class FunDep2 a | -> a

class FunDep3 a b | -> a b
