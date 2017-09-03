module Otherlib where
  
data String
data Int
data Maybe a = Just a | Nothing
data Bool = True | False

class Show a

fromJust :: Maybe a -> a
fromJust (Just a) = a
