{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Language.Haskell.Scope.Monad where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.Writer         (MonadWriter, Writer,
                                               WriterT (..), runWriter, tell)
import           Data.Data
import           Data.List                    (nub, nubBy)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Monoid                  (Monoid (..))
import           Data.Semigroup               (Semigroup (..))
import           GHC.Generics
import           Language.Haskell.Exts.SrcLoc (SrcSpan (..), SrcSpanInfo (..),
                                               noSrcSpan)
import           Language.Haskell.Exts.Syntax hiding (NewType)

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
    _                                                            -> "Main"


-----------------------------------------------------------
-- Types and Monad

data Origin = Origin NameInfo SrcSpanInfo
    deriving ( Eq, Ord, Show, Data, Generic )

data NameInfo
    = Resolved Entity
    | Binding Entity
    | None
    | ScopeError ScopeError
    deriving ( Show, Eq, Ord, Data, Generic )

data Entity = Entity
  { entityLocation :: Location
  , entitySrcSpan  :: SrcSpanInfo
  , entityName     :: QualifiedName
  , entityKind     :: EntityKind
  } deriving ( Show, Eq, Ord, Data, Generic )

entityNamespace :: Entity -> RNamespace
entityNamespace = entityKindNamespace . entityKind

data EntityKind
  = Value
  | Method
  | Selector
  | Constructor
  | Type
  | TypeVariable
  | Data -- [Entity]
  | NewType
  | TypeFamily
  | DataFamily
  | Class
  | PatternConstructor
  | PatternSelector
    deriving ( Show, Eq, Ord, Data, Generic )

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

type Location = [String]

-- data GlobalName = GlobalName Location SrcSpanInfo QualifiedName
--     deriving ( Show, Eq, Ord )

data QualifiedName = QualifiedName
    { qnameModule     :: String
    , qnameIdentifier :: String }
    deriving ( Eq, Ord, Show, Data, Generic )

-- globalNameSrcSpanInfo :: GlobalName -> SrcSpanInfo
-- globalNameSrcSpanInfo (GlobalName _ src _) = src

-- globalNameIdentifier :: GlobalName -> String
-- globalNameIdentifier (GlobalName _ _ (QualifiedName _mod ident)) = ident

entityNameIdentifier :: Entity -> String
entityNameIdentifier Entity{entityName=QualifiedName _mod ident} = ident

type Interface = [Entity]

-- Used for error reporting.
data Source
    = ImplicitSource -- Imported implicitly, usually from Prelude.
    | LocalSource -- Not imported, defined locally.
    | ModuleSource String -- Module name
    deriving ( Eq, Ord, Show, Data, Generic )

data ScopedName = ScopedName Source Entity
    deriving ( Eq, Ord, Show, Data, Generic )

data ScopeError
    = ENotInScope SrcSpanInfo QualifiedName EntityKind
    | EAmbiguous SrcSpanInfo [ScopedName]
    | EConflicting [ScopedName]
    | ETypeAsClass
    | ENotExported
    | EModNotFound
    | EInternal
    deriving ( Eq, Ord, Show, Data, Generic )
data RNamespace
    = NsTypes
    | NsTypeVariables
    | NsValues
    deriving ( Eq, Ord, Show, Data, Generic )
data Scope = Scope
    { scopeTypes  :: Map QualifiedName [ScopedName]
    -- XXX: limitTyVarScope requires tyvars to be separate from types.
    --      Sigh.
    , scopeTyVars :: Map QualifiedName [ScopedName]
    , scopeValues :: Map QualifiedName [ScopedName]
    }

emptyScope :: Scope
emptyScope = Scope
    { scopeTypes        = Map.empty
    , scopeTyVars       = Map.empty
    , scopeValues       = Map.empty}

localEntities :: Scope -> [Entity]
localEntities (Scope tys tyvars values) =
    concatMap getLocals $
      Map.elems tys ++
      Map.elems tyvars ++
      Map.elems values
  where
    getLocals []                                 = []
    getLocals (ScopedName LocalSource entity:xs) = entity:getLocals xs
    getLocals (_:xs)                             = getLocals xs

-- Bah, come up with a better name than 'Out'
data Out = Out Scope [ScopeError]
instance Semigroup Out where
  Out a aerrs <> Out b berrs = Out Scope
      { scopeTypes      = Map.unionWithKey check (scopeTypes a) (scopeTypes b)
      , scopeTyVars     = Map.unionWithKey check (scopeTyVars a) (scopeTyVars b)
      , scopeValues     = Map.unionWithKey check (scopeValues a) (scopeValues b) }
      (conflictingValues ++ conflictingTypes ++ aerrs ++ berrs)
    where
      check _k a' b' = nubBy (\a b -> getEntity a == getEntity b) (a' ++ b')
      conflictingTypes =
        [ EConflicting dups
        | dups <- Map.elems (Map.intersectionWith (++) (scopeTypes a) (scopeTypes b))
        , not (null $ drop 1 $ nub $ map getEntity dups) ]
      conflictingValues =
        [ EConflicting dups
        | dups <- Map.elems (Map.intersectionWith (++) (scopeValues a) (scopeValues b))
        , not (null $ drop 1 $ nub $ map getEntity dups) ]

instance Monoid Out where
    mempty = Out Scope
        { scopeTypes        = Map.empty
        , scopeTyVars       = Map.empty
        , scopeValues       = Map.empty} []
    mappend = (<>)

getEntity :: ScopedName -> Entity
getEntity (ScopedName _ e) = e


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
  , readerContext    :: ResolveContext
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

runRename :: ResolveEnv -> Rename a -> (Scope, [ScopeError], a)
runRename resolveEnv action = (scope, errs, a)
  where
    (a, Out scope errs) = runWriter (runReaderT (unRename action) readerEnv)
    readerEnv = ReaderEnv
      { readerResolveEnv = resolveEnv
      , readerScope      = scope
      , readerLocation   = []
      , readerModuleName = ""
      , readerContext    = ResolveToplevel }

setModuleName :: String -> Rename a -> Rename a
setModuleName name = local (\env -> env{ readerModuleName = name })

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

askContext :: Rename ResolveContext
askContext = asks readerContext

localContext :: ResolveContext -> Rename a -> Rename a
localContext ctx = local (\env -> env{readerContext = ctx})

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
            [ (QualifiedName modName ident, [ScopedName src entity])
            | entity <- iface
            , entityNamespace entity == NsTypes
            , let ident = entityNameIdentifier entity ]
        , scopeValues = Map.fromList
            [ (QualifiedName modName ident, [ScopedName src entity])
            | entity <- iface
            , entityNamespace entity == NsValues
            , let ident = entityNameIdentifier entity]
        } []

withLimitedScope :: String -> Interface -> Rename a -> Rename a
withLimitedScope modName iface =
  local $ \env ->
    env{ readerScope =
            emptyScope
                { scopeTypes = Map.fromList
                    [ (QualifiedName "" ident, [ScopedName src entity])
                    | entity <- iface
                    , entityNamespace entity == NsTypes
                    , let ident = entityNameIdentifier entity ]
                , scopeValues = Map.fromList
                    [ (QualifiedName "" ident, [ScopedName src entity])
                    | entity <- iface
                    , entityNamespace entity == NsValues
                    , let ident = entityNameIdentifier entity]
                }
       }
  where
   src = ModuleSource modName

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
getNameIdentifier (Ident _ ident)   = ident
getNameIdentifier (Symbol _ symbol) = symbol

matchName :: Match l -> Name l
matchName (Match _span name _pats _rhs _binds)             = name
matchName (InfixMatch _span _left name _right _rhs _binds) = name

qnameToEntity :: QName Origin -> Maybe Entity
qnameToEntity qname =
    case qname of
      Qual _src _mod name      -> expectResolved (ann name)
      UnQual _src name         -> expectResolved (ann name)
      Special _src _specialCon -> Nothing
  where
    expectResolved :: Origin -> Maybe Entity
    expectResolved (Origin (Resolved entity) _) = Just entity
    -- expectResolved (Origin (Binding entity) _) = Just entity
    expectResolved _                            = Nothing

nameToEntity :: Name Origin -> Maybe Entity
nameToEntity name = expectResolved (ann name)
  where
    expectResolved :: Origin -> Maybe Entity
    expectResolved (Origin (Resolved entity) _) = Just entity
    -- expectResolved (Origin (Binding entity) _) = Just entity
    expectResolved _                            = Nothing
