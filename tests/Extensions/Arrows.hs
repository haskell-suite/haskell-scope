{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Arrows where

p = proc i -> do
      v <- p -< i
      p -< ()

