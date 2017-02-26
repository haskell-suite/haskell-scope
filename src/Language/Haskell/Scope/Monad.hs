{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.Scope.Monad where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.Writer         (MonadWriter, Writer,
                                               WriterT (..), runWriter, tell)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Monoid                  (Monoid (..))
import           Language.Haskell.Exts.SrcLoc (SrcSpanInfo (..))
import           Language.Haskell.Exts.Syntax

-- ModuleName -> Interface
type ResolveEnv = Map String Interface

emptyResolveEnv :: ResolveEnv
emptyResolveEnv = Map.empty

addInterface :: String -> Interface -> ResolveEnv -> ResolveEnv
addInterface = Map.insert

fromInterfaces :: [(String, Interface)] -> ResolveEnv
fromInterfaces = Map.fromList

lookupInterface :: String -> ResolveEnv -> Maybe Interface
lookupInterface = Map.lookup




getModuleName :: Module a -> String
getModuleName m =
    case m of
        Module _ (Just (ModuleHead _ (ModuleName _ name) _ _)) _ _ _ -> name
        _   -> undefined


-----------------------------------------------------------
-- Types and Monad

data Origin = Origin NameInfo SrcSpanInfo
    deriving ( Eq, Ord, Show )

data NameInfo
    = Resolved GlobalName
    | Binding GlobalName
    | None
    | ScopeError ScopeError
    deriving ( Show, Eq, Ord )

type Location = [String]

data GlobalName = GlobalName Location QualifiedName
    deriving ( Show, Eq, Ord )

-- globalNameSrcSpanInfo :: GlobalName -> SrcSpanInfo
-- globalNameSrcSpanInfo (GlobalName src _) = src

globalNameIdentifier :: GlobalName -> String
globalNameIdentifier (GlobalName _ (QualifiedName _mod ident)) = ident

-- data Fixity = Fixity Assoc (Maybe Precedence) GlobalName
-- data Assoc = AssocNone | AssocLeft | AssocRight
-- type Precedence = Int

data Interface =
    Interface
    { ifaceValues       :: [GlobalName]
    -- , ifaceFixities     :: [Fixity]
    , ifaceTypes        :: [(GlobalName, [GlobalName])]
    , ifaceConstructors :: [(GlobalName, [GlobalName])]
    , ifaceClasses      :: [(GlobalName, [GlobalName])]
    }

-- Used for error reporting.
data Source
    = ImplicitSource -- Imported implicitly, usually from Prelude.
    | LocalSource -- Not imported, defined locally.
    | ModuleSource (ModuleName Origin)
    deriving ( Eq, Ord, Show )
data ScopedName = ScopedName Source GlobalName
    deriving ( Eq, Ord, Show )

data ScopeError
    = ENotInScope QualifiedName RNamespace
    | EAmbiguous [ScopedName]
    | ETypeAsClass
    | ENotExported
    | EModNotFound
    | EInternal
    deriving ( Eq, Ord, Show )
data QualifiedName = QualifiedName
    { qnameModule     :: String
    , qnameIdentifier :: String }
    deriving ( Eq, Ord, Show )
data RNamespace
    = NsTypes
    | NsTypeVariables
    | NsValues
    deriving ( Eq, Ord, Show )
data Scope = Scope
    { scopeTypes  :: Map QualifiedName [ScopedName]
    -- XXX: limitTyVarScope requires tyvars to be separate from types.
    --      Sigh.
    , scopeTyVars :: Map QualifiedName [ScopedName]
    , scopeValues :: Map QualifiedName [ScopedName]
    , scopeErrors :: [(SrcSpanInfo, ScopeError)]
    }
instance Monoid Scope where
    mempty = Scope
        { scopeTypes        = Map.empty
        , scopeTyVars       = Map.empty
        , scopeValues       = Map.empty
        , scopeErrors       = [] }
    mappend a b = Scope
        { scopeTypes      = scopeTypes a `Map.union` scopeTypes b
        , scopeTyVars     = scopeTyVars a `Map.union` scopeTyVars b
        , scopeValues     = Map.unionWithKey check (scopeValues a) (scopeValues b)
        , scopeErrors     = scopeErrors a ++ scopeErrors b }
      where
        check _k a' b' = a' ++ b'

-- Join two scopes, names in the first scope will shadow names in the second.
shadowJoin :: Scope -> Scope -> Scope
shadowJoin a b = Scope
    { scopeTypes      = scopeTypes a `Map.union` scopeTypes b
    , scopeTyVars     = scopeTyVars a `Map.union` scopeTyVars b
    , scopeValues     = Map.union (scopeValues a) (scopeValues b)
    , scopeErrors     = scopeErrors a ++ scopeErrors b }

data ReaderEnv = ReaderEnv
  { readerResolveEnv :: ResolveEnv
  , readerScope      :: Scope
  , readerLocation   :: Location
  , readerModuleName :: String
  }

newtype Rename a = Rename { unRename :: ReaderT ReaderEnv (Writer Scope) a }
    deriving
        ( Monad, MonadReader ReaderEnv, MonadWriter Scope
        , Functor, Applicative )

data ResolveContext = ResolveToplevel | ResolveClass | ResolveInstance
    deriving ( Show, Eq )

type Resolve a = a SrcSpanInfo -> Rename (a Origin)

-----------------------------------------------------------
-- Utilities

runRename :: ResolveEnv -> Rename a -> ([(SrcSpanInfo,ScopeError)], a)
runRename resolveEnv action = (scopeErrors scope, a)
  where
    (a, scope) = runWriter (runReaderT (unRename action) readerEnv)
    readerEnv = ReaderEnv
      { readerResolveEnv = resolveEnv
      , readerScope      = scope
      , readerLocation   = []
      , readerModuleName = "" }

getLocation :: Rename Location
getLocation = asks readerLocation

pushLocation :: String -> Rename a -> Rename a
pushLocation pos = local (\env -> env{readerLocation = pos : readerLocation env})

mapMWithLimit :: String -> (a -> Rename b) -> [a] -> Rename [b]
mapMWithLimit loc fn lst = mapM worker (zip [0::Int ..] lst)
  where
    worker (n, elt) = limitScope (loc ++ show n) (fn elt)

mapMWithLocation :: String -> (a -> Rename b) -> [a] -> Rename [b]
mapMWithLocation loc fn lst = mapM worker (zip [0::Int ..] lst)
  where
    worker (n, elt) = pushLocation (loc ++ show n) (fn elt)

-- getTvRoot :: Rename (Maybe SrcSpanInfo)
-- getTvRoot = asks (scopeTvRoot . snd)

-- withTvRoot :: SrcSpanInfo -> Rename a -> Rename a
-- withTvRoot root = local $ \(resolveEnv, env) -> (resolveEnv, env{ scopeTvRoot = Just root })

getInterface :: String -> Rename Interface
getInterface modName = do
    resolveEnv <- asks readerResolveEnv
    case lookupInterface modName resolveEnv of
        Nothing -> error $ "Language.Haskell.Scope.getInterface: module not found: " ++ modName
        Just iface -> return iface

addToScope :: Source -> String -> Interface -> Rename ()
addToScope src modName iface =
    tell mempty
        { scopeTypes = Map.fromList
            [ (QualifiedName modName ident, [ScopedName src gname])
            | (gname, _) <- ifaceTypes iface
            , let ident = globalNameIdentifier gname ]
                   -- :: Map QualifiedName [ScopedName]
        , scopeValues = Map.fromList
            [ (QualifiedName modName ident, [ScopedName src gname])
            | gname <- ifaceValues iface
            , let ident = globalNameIdentifier gname]
        }
    --      ifaceValues       :: [GlobalName]
    -- , ifaceTypes        :: [(GlobalName, [GlobalName])]
    -- ,

withLimitedScope :: Interface -> Rename a -> Rename a
withLimitedScope = undefined

-- Run action without letting the tyVars escsape the scope.
limitTyVarScope :: String -> Rename a -> Rename a
limitTyVarScope loc action = pushLocation loc $ Rename $ ReaderT $ \readerEnv ->
    let scope = readerScope readerEnv
        inScope = scope{ scopeTyVars = scopeTyVars scope `Map.union` scopeTyVars nestedScope}
        (a, nestedScope) = runWriter $ runReaderT (unRename action) readerEnv{readerScope = inScope}
    in WriterT $ Identity (a, nestedScope{ scopeTyVars = Map.empty })

limitScope :: String -> Rename a -> Rename a
limitScope loc action = pushLocation loc $ Rename $ ReaderT $ \readerEnv ->
    let scope = readerScope readerEnv
        inScope = (nestedScope `shadowJoin` scope){ scopeErrors = []}
        (a, nestedScope) = runWriter $ runReaderT (unRename action) readerEnv{readerScope = inScope}
    in WriterT $ Identity (a, mempty{ scopeErrors = scopeErrors nestedScope})

getNameIdentifier :: Name l -> String
getNameIdentifier (Ident _ ident) = ident
getNameIdentifier (Symbol _ symbol) = symbol

matchName :: Match l -> Name l
matchName (Match _span name _pats _rhs _binds) = name
matchName (InfixMatch _span _left name _right _rhs _binds) = name
