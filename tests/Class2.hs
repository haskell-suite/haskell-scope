module Class2 where

data String
data List a

class Show a where
  show :: a -> String
  showList :: List a -> String

  showList = show
