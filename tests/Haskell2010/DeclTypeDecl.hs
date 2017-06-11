{-# LANGUAGE Haskell2010 #-}
module DeclTypeDecl where

type DHead = ()

type a `Infix1` b = Infix2 a b
type a `Infix2` b = Infix1 a b

type (Parens) a = DHead a
