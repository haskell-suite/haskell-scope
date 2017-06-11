{-# LANGUAGE Haskell2010 #-}
module DeclFunBind where

data Bool = True | False
data Maybe a = Just a | Nothing

id a = a

a `infix1` b = ()
a `infix1` b = ()
  where c = a b
a `infix1` b
  | a = b
  | b = a

a + b = ()
Just a + Just b = ()

(/) False = ()
(/) a = ()

(//) False Nothing = ()
(//) a b = ()

False - False = ()
a - b = ()

not True = False
not False = True
