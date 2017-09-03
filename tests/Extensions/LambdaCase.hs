{-# LANGUAGE LambdaCase #-}
module LambdaCase where

data Bool = True | False

not = \case
  True -> False
  False -> True
