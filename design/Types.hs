{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
module Types where

import Data.Map (Map)
import qualified Data.Map as Map

-----------------------------------------------------------------------
-- Scoping

data NameInfo
    = Resolved Entity
    | Binding Entity
    | None
    | ScopeError ScopeError
    deriving ( Show, Eq, Ord )

data Entity = Entity
  { entityLocation :: Location
  , entityName     :: String
  , entityKind     :: EntityKind
  } deriving ( Show, Eq, Ord )

entityNamespace :: Entity -> RNamespace
entityNamespace = entityKindNamespace . entityKind

entityKindNamespace :: EntityKind -> RNamespace
entityKindNamespace eKind =
  case eKind of
    Value              -> NsValues
    Method             -> NsValues
    Selector           -> NsValues
    Constructor        -> NsValues
    Type               -> NsTypes
    TypeVariable       -> NsTypeVariables
    Data               -> NsTypes
    NewType            -> NsTypes
    TypeFamily         -> NsTypes
    DataFamily         -> NsTypes
    Class              -> NsTypes
    PatternConstructor -> NsTypes
    PatternSelector    -> NsTypes

data EntityKind
  = Value
  | Method
  | Selector
  | Constructor
  | Type
  | TypeVariable
  | Data
  | NewType
  | TypeFamily
  | DataFamily
  | Class
  | PatternConstructor
  | PatternSelector
    deriving ( Show, Eq, Ord )

type Location = [String]

data Source
    = ImplicitSource -- Imported implicitly, usually from Prelude.
    | LocalSource -- Not imported, defined locally.
    | ModuleSource String -- Module name
    deriving ( Eq, Ord, Show )

data ScopedName = ScopedName Source Entity
    deriving ( Eq, Ord, Show )

data ScopeError
    = ENotInScope String EntityKind
    | EAmbiguous [ScopedName]
    | EConflicting [ScopedName]
    | ETypeAsClass
    | ENotExported
    | EModNotFound
    | EInternal
    | ELazy [ScopeError]
    deriving ( Eq, Ord, Show )
data RNamespace
    = NsTypes
    | NsTypeVariables
    | NsValues
    deriving ( Eq, Ord, Show )

data Scope = Scope
    { scopeTypes  :: !(Map String [ScopedName])
    -- XXX: limitTyVarScope requires tyvars to be separate from types.
    --      Sigh.
    , scopeTyVars :: !(Map String [ScopedName])
    , scopeValues :: !(Map String [ScopedName])
    } deriving (Show)

instance Monoid Scope where
  mempty = Scope Map.empty Map.empty Map.empty
  mappend (Scope t1 tv1 v1) (Scope t2 tv2 v2) =
    Scope (Map.unionWith (++) t1 t2)
          (Map.unionWith (++) tv1 tv2)
          (Map.unionWith (++) v1 v2)

addToScope :: String -> ScopedName -> Scope -> Scope
addToScope k v s = s{scopeValues = Map.insertWith (++) k [v] (scopeValues s)}
