module Language.Haskell.Scope
  ( ResolveEnv
  , emptyResolveEnv
  , lookupInterface
  , fromInterfaces
  , getModuleName

  , Origin(..)
  , NameInfo(..)
  , Location
  , Entity(..)
  , EntityKind(..)
  , QualifiedName(..)
  , ScopedName(..)
  , RNamespace(..)
  , Interface(..)
  , Source(..)
  , ScopeError(..)
  , resolve

  ) where

import Language.Haskell.Scope.Monad
import Language.Haskell.Scope.Resolve
