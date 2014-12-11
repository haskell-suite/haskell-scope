module Shadowing2 where

y = y

x y = y
  where
    y = 10
