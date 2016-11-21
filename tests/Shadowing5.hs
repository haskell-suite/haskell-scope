module Shadowing4 where

x = case x of
      x -> x
  where
    x = 10
