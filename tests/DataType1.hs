module DataType1 where

data Int

data Data a = Con1 a | Con2 Int | Con3 a Int | Con4 (Data a) | Con5 (Data Int)
