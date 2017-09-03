{-# LANGUAGE ParallelListComp #-}
module ParallelListComp where

lst = [ x+y | x <- lst | y <- lst ]
