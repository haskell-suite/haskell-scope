{-# LANGUAGE Haskell2010 #-}
module Type where

data Maybe a
data Int
class Show a

forall :: Show a => a
forall = forall

tyfun :: a -> b
tyfun = tyfun

tytuple :: (a,b)
tytuple = tytuple

tylist :: [a]
tylist = tylist

-- typararray

tyapp :: a b c
tyapp = tyapp

tycon :: Maybe Int
tycon = tycon

typaren :: (Int) -> (Maybe a)
typaren = typaren

tyinfix :: a `Maybe` b
tyinfix = tyinfix

-- tykind
-- typromoted
-- tyequals
-- tysplice
-- tybang
-- tywildcard
-- tyquasiquote
