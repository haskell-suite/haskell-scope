module Language.Haskell.Scope
  ( ResolveEnv
  , emptyResolveEnv
  , lookupInterface
  , fromInterfaces
  , getModuleName

  , Origin(..)
  , NameInfo(..)
  , Location
  , GlobalName(..)
  , QualifiedName(..)
  , ScopedName(..)
  , RNamespace(..)
  , Interface(..)
  , Source(..)
  , ScopeError(..)
  , resolve
  , globalNameSrcSpanInfo

  ) where

import Language.Haskell.Scope.Monad
import Language.Haskell.Scope.Resolve
