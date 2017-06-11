{-# LANGUAGE Haskell2010 #-}
module DeclDataDecl where

class Show a

data T1
data T2 a
data T3 a = T3 a
data T4 deriving Show
data T5 = T5 deriving Show

newtype T6 = T6 T1
newtype T7 a = T7 (T2 a)
newtype T8 = T8 T1 deriving Show

data T9 a b = Left a | Right b

data T10 a b = T10 { field1 :: a, field2 :: b}
data Show a => T11 a = T11 a

-- Not well-typed. Just used to verify name resolution.
collection :: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)
collection = [T3, T5, T6, T7, T8, Left, Right, T10, field1, field2, T11]
