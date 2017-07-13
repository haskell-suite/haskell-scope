{-# LANGUAGE Haskell2010 #-}
module DeclInfixDecl where

infix +
infixr +
infixl +

infix 1 +
infixr 1 +
infixl 1 +

(+) = (+)
