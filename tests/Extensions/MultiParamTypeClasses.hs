{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MultiParamTypeClasses where

class HasBounds a
class Monad m

data Array
data UArray
data Bool
data Char

class HasBounds a => IArray a e
class (HasBounds a, Monad m) => MArray a e m

instance IArray Array e
instance IArray UArray Bool
instance IArray UArray Char
