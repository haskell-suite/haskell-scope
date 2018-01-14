{- LANGUAGE PatternGuards -}
module PatternGuards where

fn a
  | Just y <- Nothing
  , False <- True
  = fn a
  | True = False
