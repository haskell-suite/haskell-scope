{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.Scope.Monad where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.Writer         (MonadWriter, Writer,
                                               WriterT (..), runWriter, tell)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Monoid                  (Monoid (..))
import           Language.Haskell.Exts.SrcLoc (SrcSpanInfo (..), noSrcSpan)
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

data GlobalName = GlobalName Location SrcSpanInfo QualifiedName
    deriving ( Show, Eq, Ord )

globalNameSrcSpanInfo :: GlobalName -> SrcSpanInfo
globalNameSrcSpanInfo (GlobalName _ src _) = src

globalNameIdentifier :: GlobalName -> String
globalNameIdentifier (GlobalName _ _ (QualifiedName _mod ident)) = ident

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
    = ENotInScope SrcSpanInfo QualifiedName RNamespace
    | EAmbiguous SrcSpanInfo [ScopedName]
    | EConflicting [ScopedName]
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
    -- , scopeErrors :: [ScopeError]
    }

emptyScope :: Scope
emptyScope = Scope
    { scopeTypes        = Map.empty
    , scopeTyVars       = Map.empty
    , scopeValues       = Map.empty}

-- Bah, come up with a better name than 'Out'
data Out = Out Scope [ScopeError]
instance Monoid Out where
    mempty = Out Scope
        { scopeTypes        = Map.empty
        , scopeTyVars       = Map.empty
        , scopeValues       = Map.empty} []
    mappend (Out a aerrs) (Out b berrs) = Out Scope
        { scopeTypes      = Map.unionWithKey check (scopeTypes a) (scopeTypes b)
        , scopeTyVars     = Map.unionWithKey check (scopeTyVars a) (scopeTyVars b)
        , scopeValues     = Map.unionWithKey check (scopeValues a) (scopeValues b) }
        (conflictingValues ++ conflictingTypes ++ aerrs ++ berrs)
      where
        check _k a' b' = a' ++ b'
        conflictingTypes =
          [ EConflicting dups
          | dups <- Map.elems (Map.intersectionWith (++) (scopeTypes a) (scopeTypes b)) ]
        conflictingValues =
          [ EConflicting dups
          | dups <- Map.elems (Map.intersectionWith (++) (scopeValues a) (scopeValues b)) ]

-- Join two scopes, names in the first scope will shadow names in the second.
shadowJoin :: Scope -> Scope -> Scope
shadowJoin a b = Scope
    { scopeTypes      = scopeTypes a `Map.union` scopeTypes b
    , scopeTyVars     = scopeTyVars a `Map.union` scopeTyVars b
    , scopeValues     = Map.union (scopeValues a) (scopeValues b) }

data ReaderEnv = ReaderEnv
  { readerResolveEnv :: ResolveEnv
  , readerScope      :: Scope
  , readerLocation   :: Location
  , readerModuleName :: String
  }

newtype Rename a = Rename { unRename :: ReaderT ReaderEnv (Writer Out) a }
    deriving
        ( Monad, MonadReader ReaderEnv, MonadWriter Out
        , Functor, Applicative )

data ResolveContext = ResolveToplevel | ResolveClass | ResolveInstance
    deriving ( Show, Eq )

type Resolve a = a SrcSpanInfo -> Rename (a Origin)

-----------------------------------------------------------
-- Utilities

runRename :: ResolveEnv -> Rename a -> ([ScopeError], a)
runRename resolveEnv action = (errs, a)
  where
    (a, Out scope errs) = runWriter (runReaderT (unRename action) readerEnv)
    readerEnv = ReaderEnv
      { readerResolveEnv = resolveEnv
      , readerScope      = scope
      , readerLocation   = []
      , readerModuleName = "" }

tellScopeErrors :: [ScopeError] -> Rename ()
tellScopeErrors errs = tell $ Out emptyScope errs

tellScopeValue :: QualifiedName -> ScopedName -> Rename ()
tellScopeValue qname resolved =
  tell $ Out emptyScope{ scopeValues = Map.singleton qname [resolved]} []

tellScopeType :: QualifiedName -> ScopedName -> Rename ()
tellScopeType qname resolved =
  tell $ Out emptyScope{ scopeTypes = Map.singleton qname [resolved]} []

tellScopeTyVar :: QualifiedName -> ScopedName -> Rename ()
tellScopeTyVar qname resolved =
  tell $ Out emptyScope{ scopeTyVars = Map.singleton qname [resolved]} []


getLocation :: Rename Location
getLocation = asks readerLocation

pushLocation :: String -> Rename a -> Rename a
pushLocation pos = local (\env -> env{readerLocation = pos : readerLocation env})

mapMWithLimit :: String -> (a -> Rename b) -> [a] -> Rename [b]
mapMWithLimit loc fn lst = mapM worker (zip [0::Int ..] lst)
  where
    worker (n, elt) = limitScope (loc ++ show n) (fn elt)

numberedLocations :: String -> [Rename a] -> [Rename a]
numberedLocations loc lst = map worker (zip [0::Int ..] lst)
  where
    worker (n, action) = pushLocation (loc ++ show n) action

{-# INLINE mapMWithLocation #-}
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
    tell $ Out emptyScope
        { scopeTypes = Map.fromList
            [ (QualifiedName modName ident, [ScopedName src gname])
            | (gname, _) <- ifaceTypes iface
            , let ident = globalNameIdentifier gname ]
                   -- :: Map QualifiedName [ScopedName]
        , scopeValues = Map.fromList
            [ (QualifiedName modName ident, [ScopedName src gname])
            | gname <- ifaceValues iface
            , let ident = globalNameIdentifier gname]
        } []

withLimitedScope :: Interface -> Rename a -> Rename a
withLimitedScope = undefined

-- Run action without letting the tyVars escsape the scope.
limitTyVarScope :: String -> Rename a -> Rename a
limitTyVarScope loc action = {-pushLocation loc $-} Rename $ ReaderT $ \readerEnv ->
    let scope = readerScope readerEnv
        inScope = scope{ scopeTyVars = scopeTyVars scope `Map.union` scopeTyVars nestedScope}
        (a, Out nestedScope errs) = runWriter $ runReaderT (unRename action) readerEnv{readerScope = inScope}
    in WriterT $ Identity (a, Out nestedScope{ scopeTyVars = Map.empty } errs)

limitScope :: String -> Rename a -> Rename a
limitScope loc action = pushLocation loc $ Rename $ ReaderT $ \readerEnv ->
    let scope = readerScope readerEnv
        inScope = (nestedScope `shadowJoin` scope)
        (a, Out nestedScope errs) = runWriter $ runReaderT (unRename action) readerEnv{readerScope = inScope}
    in WriterT $ Identity (a, Out emptyScope errs)

recursiveScope :: Rename a -> Rename a
recursiveScope action = Rename $ ReaderT $ \readerEnv ->
    let scope = readerScope readerEnv
        inScope = (nestedScope `shadowJoin` scope)
        (a, Out nestedScope errs) = runWriter $ runReaderT (unRename action) readerEnv{readerScope = inScope}
    in WriterT $ Identity (a, Out nestedScope errs)


shadowSequence :: [Rename a] -> Rename [a]
shadowSequence lst = shadowSequence' pure [] [] lst

withShadowSequence :: [Rename a] -> ([a] -> Rename b) -> Rename b
withShadowSequence lst cont = shadowSequence' cont [] [] lst


shadowSequence' :: ([a] -> Rename b) -> [[ScopeError]] -> [a] -> [Rename a] -> Rename b
shadowSequence' cont errs acc lst = Rename $ ReaderT $ \readerEnv -> readerEnv `seq`
  case lst of
    [] -> runReaderT (unRename $ do
                        tell $ Out emptyScope (concat errs)
                        cont (reverse acc))
                      readerEnv
    (x:xs) ->
      case runWriter $ runReaderT (unRename x) readerEnv of
        (x_ret, Out x_scope x_errs) ->
          let scope = readerScope readerEnv
              inScope = (x_scope `shadowJoin` scope)
          in runReaderT
                (unRename $ shadowSequence' cont (x_errs:errs) (x_ret:acc) xs)
                readerEnv{readerScope = inScope}

restrictScope :: QualifiedName -> ScopedName -> Rename a -> Rename a
restrictScope qname scopedName =
  local $ \env ->
    let s = readerScope env
        s' = s{scopeValues = Map.adjust (const [scopedName]) qname (scopeValues s)}
    in env{readerScope = s'}

getNameIdentifier :: Name l -> String
getNameIdentifier (Ident _ ident) = ident
getNameIdentifier (Symbol _ symbol) = symbol

matchName :: Match l -> Name l
matchName (Match _span name _pats _rhs _binds) = name
matchName (InfixMatch _span _left name _right _rhs _binds) = name
