{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
module Design where

import Data.Map (Map)
import qualified Data.Map as Map

-----------------------------------------------------------------------
-- Language
data Name l = Name l String
data Declaration l
  = FunDecl (Name l) [Pattern l] (Expr l)
  | DataDecl (Name l) [ConDecl l]

data ConDecl l = ConDecl (Name l) [Field l]

type Field l = Name l

data Pattern l
  = PatVar (Name l)
  | PatField (Name l) (Name l) (Name l) -- A{b=c}
  | PatWildcard (Name l) -- A{..}

data Expr l
  = Variable (Name l)
  | Application (Expr l) (Expr l)

type Module l = [Declaration l]



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
    }

emptyScope :: Scope
emptyScope = Scope Map.empty Map.empty Map.empty

-----------------------------------------------------------------------
-- Monad

data ReaderEnv = ReaderEnv
  { readerScope      :: Scope -- lazy
  , readerIncScope   :: !Scope -- strict
  , readerLocation   :: Location -- strict
  , readerErrors     :: ![ScopeError] -- strict
  }


--FinalScope -> IncrementalScope -> [ScopeError] -> (# a, [ScopeError], Inner, Outer #)
--newtype Rename a = Rename { unRename :: ReaderT ReaderEnv (Writer Out) a }
newtype Rename a = Rename { unRename :: ReaderEnv -> (# a, [ScopeError], Scope, Scope #)}

instance Monad Rename where
  return a = Rename $ \ReaderEnv{..} -> (# a, readerErrors, emptyScope, emptyScope #)
  (>>=) = undefined

instance Applicative Rename where
  pure = undefined
  (<*>) = undefined

instance Functor Rename where
  fmap = undefined

-----------------------------------------------------------------------
-- Scoping AST

scopeDeclaration decl =
  case decl of
    FunDecl name patterns expr -> limitInner $
      FunDecl
        <$> bindOuter name
        <*> mapM scopePattern patterns
        <*> scopeExpr expr

scopePattern pattern =
  case pattern of
    PatVar name -> PatVar <$> bindInner name
    -- PatField (Name l) (Name l) (Name l) -- A{b=c}
    -- PatWildcard (Name l) -- A{..}

scopeExpr expr =
  case expr of
    Variable name -> Variable <$> resolveName name
    Application a b -> Application <$> scopeExpr a <*> scopeExpr b

limitInner :: Rename a -> Rename a
limitInner = undefined
bindOuter = undefined
bindInner = undefined
resolveName :: Name l -> Rename (Name NameInfo)
resolveName = undefined

-----------------------------------------------------------------------
-- Test cases

-- #1
-- data Data = Field {field}
-- fn Data{bad = name} = name

-- #2
-- data Data = Field {field :: Data}
-- fn Data{..} = field

-- #3
-- data Data = Field {field :: Data}
-- fn dat = field dat

-- #4
-- fn a a = c
