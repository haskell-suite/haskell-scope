module AST where

import Data.String

data Name l = Name l String deriving (Show)
instance IsString (Name a) where
  fromString = Name undefined

data Declaration l
  = FunDecl (Name l) [Pattern l] (Expr l)
  | DataDecl (Name l) [ConDecl l]
  deriving (Show)

data ConDecl l = ConDecl (Name l) [Field l]
  deriving (Show)

type Field l = Name l

data Pattern l
  = PatVar (Name l)
  | PatField (Name l) (Name l) (Name l) -- A{b=c}
  | PatWildcard (Name l) -- A{..}
    deriving (Show)

data Expr l
  = Variable (Name l)
  | Application (Expr l) (Expr l)
    deriving (Show)

newtype Module l = Module [Declaration l]
  deriving (Show)
