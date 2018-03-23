{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Monad where

import Data.Map (Map)
import qualified Data.Map as Map

import Types

-----------------------------------------------------------------------
-- Monad

data ReaderEnv = ReaderEnv
  { readerScope      :: Scope -- lazy
  , readerLocation   :: Location -- strict
  }

data StateEnv = StateEnv
  { stateInnerScope :: Scope
  , stateOuterScope :: !Scope
  , stateErrors     :: ![ScopeError] }

--FinalScope -> IncrementalScope -> [ScopeError] -> (# a, [ScopeError], Inner, Outer #)
--newtype Rename a = Rename { unRename :: ReaderT ReaderEnv (Writer Out) a }
newtype Rename a = Rename { unRename :: Scope -> Location -> StateEnv -> (# a, StateEnv #)}

instance Monad Rename where
  return a = Rename $ \_ _ st -> (# a, st #)
  f >>= g = Rename $ \scope loc st ->
    case unRename f scope loc st of
      (# a, st' #) -> unRename (g a) scope loc st'

instance Applicative Rename where
  pure = return
  f <*> g = f >>= \f' -> fmap f' g

instance Functor Rename where
  fmap f a = do
    a' <- a
    return (f a')

type Resolve a = a () -> Rename (a NameInfo)

runRename :: Rename a -> (a, [ScopeError], Scope)
runRename action =
    (ret, errs, finalScope)
  where
    !st = StateEnv mempty mempty []
    !(ret, StateEnv{stateOuterScope=finalScope, stateErrors=errs}) =
        case unRename action finalScope [] st of
          (# ret, st' #) -> (ret, st')

askLocation :: Rename Location
askLocation = Rename $ \_ loc st -> (# loc, st #)

askScope :: Rename Scope
askScope = Rename $ \scope _ st -> (# scope, st #)

tellError :: ScopeError -> Rename ()
tellError !err = Rename $ \_ _ st ->
  (# (), st{stateErrors = err:stateErrors st} #)
