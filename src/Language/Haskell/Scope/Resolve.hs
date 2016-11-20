{-
Goals:
  Intuitive interface.
  Good error messages, or at least the foundation for good error messages.
  Origin analysis of recursive modules.
  Fast.
  Support rebindable syntax.
  Support scoped type variables.

Bugs:
  Instance methods aren't resolved properly.

Notes:
  Rebindable syntax? We do not do any desugaring. Instead we just resolve
  the relevant functions and add them to the module interface. When desugaring
  with RebindableSyntax enabled, those functions can be used instead of the
  default Prelude ones.
  This wont' work. We need to inspect the scope at the locations the rebindable
  functions are used. Looking at the global scope isn't enough.

  Scope environment:
  We need to know about the fields of data construtors:
  fn DataType{..} = ...
  We need to know about the members of classes:
  instance Module.Class Ty where
    thisMethodDoesNotHaveToBeQualified = ...

-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Language.Haskell.Scope.Resolve
    ( ResolveEnv
    , emptyResolveEnv
    , lookupInterface
    , fromInterfaces
    , getModuleName

    , Origin(..)
    , NameInfo(..)
    , GlobalName(..)
    , QualifiedName(..)
    , Interface(..)
    , Source(..)
    , ScopeError(..)
    , resolve
    -- , globalNameSrcSpanInfo

    -- XXX: Dont export.
    , getNameIdentifier
    ) where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.Writer                   (MonadWriter, Writer,
                                                         WriterT (..),
                                                         runWriter, tell)
import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Data.Maybe
import           Data.Monoid                            (Monoid (..))
import           Language.Haskell.Exts.Syntax
import           Language.Haskell.Exts.SrcLoc

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



deriveInterface :: Module Origin -> Interface
deriveInterface m =
    case m of
        Module _ (Just (ModuleHead _ _ _ (Just (ExportSpecList _ exports)))) _ _ _ ->
            Interface
            { ifaceValues =
                [ gname
                | EVar _ qname <- exports
                , Origin (Resolved gname) _ <- [ann qname] ] ++
                [ gname
                | EThingWith _ _wild _qname cnames <- exports
                , ConName _ name <- cnames
                , Origin (Resolved gname) _ <- [ann name] ]

            , ifaceTypes =
                [ (gname, [])
                | EAbs _ _ns qname <- exports
                , Origin (Resolved gname) _ <- [ann qname] ] ++
                [ (gname, [])
                | EThingWith _ _wild qname _cnames <- exports
                , Origin (Resolved gname) _ <- [ann qname] ]
            , ifaceConstructors = []
            , ifaceClasses = []
            }
        _ -> error "Language.Haskell.Scope.deriveInterface: undefined"

-- FIXME: Move this to a utilities module.
getModuleName :: Module a -> String
getModuleName m =
    case m of
        Module _ (Just (ModuleHead _ (ModuleName _ name) _ _)) _ _ _ -> name

-- Resolve all names in a module
resolve :: ResolveEnv -> Module SrcSpanInfo -> (ResolveEnv, [ScopeError], Module Origin)
resolve resolveEnv m =
    let (errs, m') = runRename resolveEnv $ resolveModule m
        iface = deriveInterface m'
        name = getModuleName m
    in (addInterface name iface resolveEnv, errs, m')




-----------------------------------------------------------
-- Types and Monad

data Origin = Origin NameInfo SrcSpanInfo
    deriving ( Show )

data NameInfo
    = Resolved GlobalName
    | Binding GlobalName
    | None
    | ScopeError ScopeError
    deriving ( Show )

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
    deriving ( Show )
data ScopedName = ScopedName Source GlobalName
    deriving ( Show )

data ScopeError
    = ENotInScope QualifiedName RNamespace SrcSpanInfo
    -- | ETypeNotInScope QualifiedName SrcSpanInfo
    -- | EConstructorNotInScope QualifiedName SrcSpanInfo
    -- | ETypeVariableNotInScope QualifiedName SrcSpanInfo
    | EAmbiguous [ScopedName]
    | ETypeAsClass
    | ENotExported
    | EModNotFound
    | EInternal
    deriving ( Show )
data QualifiedName = QualifiedName
    { qnameModule     :: String
    , qnameIdentifier :: String }
    deriving ( Eq, Ord, Show )
data RNamespace
    = NsTypes
    | NsTypeVariables
    | NsValues
    deriving ( Show )
data Scope = Scope
    { scopeTypes      :: Map QualifiedName [ScopedName]
    -- XXX: limitTyVarScope requires tyvars to be separate from types.
    --      Sigh.
    , scopeTyVars     :: Map QualifiedName [ScopedName]
    , scopeValues     :: Map QualifiedName [ScopedName]
    , scopeErrors     :: [ScopeError]
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
  { readerResolveEnv  :: ResolveEnv
  , readerScope       :: Scope
  , readerLocation    :: Location
  , readerModuleName  :: String
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

runRename :: ResolveEnv -> Rename a -> ([ScopeError], a)
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
pushLocation pos = local (\reader -> reader{readerLocation = pos : readerLocation reader})

mapMWithLimit :: String -> (a -> Rename b) -> [a] -> Rename [b]
mapMWithLimit loc fn lst = mapM worker (zip [0::Int ..] lst)
  where
    worker (n, elt) = limitScope (loc ++ show n) (fn elt)

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
addToScope src modName iface = do
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
limitTyVarScope :: Rename a -> Rename a
limitTyVarScope action = Rename $ ReaderT $ \readerEnv ->
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


-----------------------------------------------------------
-- Name binding and resolution

-- FIXME: Use ETypeNotInScope, EConstructorNotInScope, etc.
resolveName :: RNamespace -> Resolve Name
resolveName = resolveName' ""

resolveName' :: String -> RNamespace -> Resolve Name
resolveName' = resolveName'' IsNotTv

data IsTv = IsTv | IsNotTv

resolveName'' :: IsTv -> String -> RNamespace -> Resolve Name
resolveName'' isTv qualification ns name =
    con <$> getScoped <*> pure nameString
  where
    (con, src, nameString) =
        case name of
            Ident a b  -> (Ident, a, b)
            Symbol a b -> (Symbol, a, b)
    qname = QualifiedName qualification nameString
    worker :: (Scope -> Map QualifiedName [ScopedName]) -> Rename Origin
    worker field = do
        m <- asks (field . readerScope)
        loc <- getLocation
        let ret = case Map.lookup qname m of
                Nothing ->
                    case isTv of
                        IsNotTv ->
                            Left (ENotInScope qname ns src)
                        IsTv ->
                            Right (ScopedName LocalSource (GlobalName loc qname))
                Just [var] -> Right var
                Just vars  -> Left (EAmbiguous vars)
            nameInfo =
                case ret of
                    Left err -> ScopeError err
                    Right (ScopedName _ gname) -> Resolved gname
        tell mempty{ scopeErrors =
                        maybeToList (either (Just) (const Nothing) ret) }
        return $ Origin nameInfo src
    getScoped =
        case ns of
            NsValues        -> worker scopeValues
            NsTypes         -> worker scopeTypes
            NsTypeVariables -> worker scopeTyVars

resolveTyVar :: Resolve Name
resolveTyVar name =
  pushLocation "tv" $ resolveName'' IsTv "" NsTypeVariables name

resolveQName :: RNamespace -> Resolve QName
resolveQName ns qname =
    case qname of
        Qual src (ModuleName l m) name ->
            Qual (Origin None src)
                <$> pure (ModuleName (Origin None l) m)
                <*> resolveName' m ns name
        UnQual src name -> do
            name' <- resolveName ns name
            let Origin origin _ = ann name'
            UnQual (Origin origin src)
                <$> pure name'
        Special src specialCon ->
            Special (Origin None src)
                <$> resolveSpecialCon specialCon

defineName :: RNamespace -> Resolve Name
defineName ns name = do
    thisModule <- asks readerModuleName
    loc <- getLocation
    let ident = getNameIdentifier name
        qname = QualifiedName "" ident
        gname = GlobalName loc (QualifiedName thisModule ident)
        resolved = ScopedName LocalSource gname
    case ns of
        NsValues ->
            tell mempty{ scopeValues = Map.singleton qname [resolved]}
        NsTypes  ->
            tell mempty{ scopeTypes = Map.singleton qname [resolved]}
        NsTypeVariables ->
            tell mempty{ scopeTyVars = Map.singleton qname [resolved] }
    case name of
      Ident src ident   -> pure $ Ident (Origin (Binding gname) src) ident
      Symbol src symbol -> pure $ Symbol (Origin (Binding gname) src) symbol

-- resolveMaybe :: Resolve a -> Resolve (Maybe a)
resolveMaybe :: (a -> Rename b) -> Maybe a -> Rename (Maybe b)
resolveMaybe fn mbValue =
    case mbValue of
        Nothing    -> return Nothing
        Just value -> Just <$> fn value

-------------------------------------------------------------
---- Name resolution

resolveSafety :: Resolve Safety
resolveSafety = pure . fmap (Origin None)

resolveCallConv :: Resolve CallConv
resolveCallConv = pure . fmap (Origin None)

resolveKind :: Resolve Kind
resolveKind kind =
    case kind of
        KindStar src -> pure $ KindStar (Origin None src)
        _ -> error "resolveKind"

resolveTyVarBind :: Resolve TyVarBind
resolveTyVarBind tyVarBind =
    case tyVarBind of
        KindedVar src name kind ->
            KindedVar
                <$> pure (Origin None src)
                <*> defineName NsTypeVariables name
                <*> resolveKind kind
        UnkindedVar src name    ->
            UnkindedVar
                <$> pure (Origin None src)
                <*> defineName NsTypeVariables name

resolveDeclHead :: Resolve DeclHead
resolveDeclHead dhead =
    case dhead of
        DHApp src dh tyVarBind ->
            DHApp
                <$> pure (Origin None src)
                <*> resolveDeclHead dh
                <*> resolveTyVarBind tyVarBind
        DHead src name ->
            DHead
                <$> pure (Origin None src)
                <*> defineName NsTypes name
        DHInfix{} -> error "resolveDeclHead"
        DHParen _ next -> resolveDeclHead next

resolveAsst :: Resolve Asst
resolveAsst asst =
    case asst of
        ClassA src qname tys ->
            ClassA (Origin None src)
                <$> resolveQName NsTypes qname
                <*> mapM resolveType tys
        _ -> error "resolveAsst"

resolveContext :: Resolve Context
resolveContext ctx =
    case ctx of
        CxSingle src asst ->
            CxSingle (Origin None src)
                <$> resolveAsst asst
        CxEmpty src -> pure (CxEmpty (Origin None src))
        _ -> error "resolveContext"

resolveOverlap :: Resolve Overlap
resolveOverlap overlap =
    case overlap of
        _ -> error "resolveOverlap"

resolveDataOrNew :: Resolve DataOrNew
resolveDataOrNew = pure . fmap (Origin None)

resolveInstHead :: Resolve InstHead
resolveInstHead instHead =
    case instHead of
        IHCon src qname ->
            IHCon (Origin None src)
                <$> resolveQName NsTypes qname
        IHInfix src ty qname ->
            IHInfix (Origin None src)
                <$> resolveType ty
                <*> resolveQName NsTypes qname
        IHApp src subHead ty ->
            IHApp (Origin None src)
                <$> resolveInstHead subHead
                <*> resolveType ty
        IHParen src sub ->
            IHParen (Origin None src)
                <$> resolveInstHead sub

resolveInstRule :: Resolve InstRule
resolveInstRule instRule =
    case instRule of
        IRule src mbTyVarBinds mbContext iHead ->
            IRule (Origin None src)
                <$> resolveMaybe (mapM resolveTyVarBind) mbTyVarBinds
                <*> resolveMaybe resolveContext mbContext
                <*> resolveInstHead iHead
        IParen src subRule ->
            IParen (Origin None src) <$> resolveInstRule subRule
        -- _ -> error "resolveInstRule"

resolveDeriving :: Resolve Deriving
resolveDeriving (Deriving src instRules) =
    Deriving
        <$> pure (Origin None src)
        <*> mapM resolveInstRule instRules

resolveSpecialCon :: Resolve SpecialCon
resolveSpecialCon specialCon = pure $
    case specialCon of
        UnitCon src             -> UnitCon $ Origin None src
        ListCon src             -> ListCon $ Origin None src
        FunCon src              -> FunCon $ Origin None src
        TupleCon src boxed size -> TupleCon (Origin None src) boxed size
        Cons src                -> Cons $ Origin None src
        UnboxedSingleCon src    -> UnboxedSingleCon $ Origin None src

resolveType :: Resolve Type
resolveType ty =
    case ty of
        TyCon src qname ->
            TyCon (Origin None src)
                <$> resolveQName NsTypes qname
        TyVar src tyVar ->
            TyVar (Origin None src)
                <$> resolveTyVar tyVar
        TyFun src a b ->
            TyFun (Origin None src)
                <$> resolveType a
                <*> resolveType b
        TyApp src a b ->
            TyApp (Origin None src)
                <$> resolveType a
                <*> resolveType b
        TyParen src sub ->
            TyParen (Origin None src)
                <$> resolveType sub
        TyTuple src boxed tys ->
            TyTuple (Origin None src) boxed
                <$> mapM resolveType tys
        TyForall src mbTyVarBinds mbCtx ty -> limitTyVarScope $
            TyForall (Origin None src)
                <$> resolveMaybe (mapM resolveTyVarBind) mbTyVarBinds
                <*> resolveMaybe resolveContext mbCtx
                <*> resolveType ty
        TyList src ty ->
            TyList (Origin None src)
                <$> resolveType ty
        _ -> error $ "resolveType: " ++ show ty

-- resolveBangType :: Resolve BangType
-- resolveBangType bangTy =
--     case bangTy of
--         BangedTy src ->
--             pure $ BangedTy (Origin None src)
--         UnpackedTy src ->
--             pure $ UnpackedTy (Origin None src)

resolveFieldDecl :: Resolve FieldDecl
resolveFieldDecl fieldDecl =
    case fieldDecl of
        FieldDecl src names ty ->
            FieldDecl (Origin None src)
                <$> mapM (defineName NsValues) names
                <*> resolveType ty

resolveConDecl :: Resolve ConDecl
resolveConDecl conDecl =
    case conDecl of
        ConDecl src name tys ->
            ConDecl (Origin None src)
                <$> defineName NsValues name
                <*> mapM resolveType tys
        RecDecl src name fieldDecls ->
            RecDecl (Origin None src)
                <$> defineName NsValues name
                <*> mapM resolveFieldDecl fieldDecls
        _ -> error "resolveConDecl"

resolveQualConDecl :: Resolve QualConDecl
resolveQualConDecl (QualConDecl src mbTyVarBinds ctx conDecl) =
    QualConDecl (Origin None src)
        <$> resolveMaybe (mapM resolveTyVarBind) mbTyVarBinds
        <*> resolveMaybe resolveContext ctx
        <*> resolveConDecl conDecl

resolveSign :: Resolve Sign
resolveSign sign = pure $
    case sign of
        Signless src -> Signless (Origin None src)
        Negative src -> Negative (Origin None src)

resolvePat :: Resolve Pat
resolvePat pat =
    case pat of
        PVar src name ->
            PVar (Origin None src)
                <$> defineName NsValues name
        PApp src con pats ->
            PApp (Origin None src)
                <$> resolveQName NsValues con
                <*> mapM resolvePat pats
        PWildCard src ->
            pure $ PWildCard (Origin None src)
        PParen src sub ->
            PParen (Origin None src)
                <$> resolvePat sub
        PTuple src boxed pats ->
            PTuple (Origin None src) boxed
                <$> mapM resolvePat pats
        PLit src sign lit ->
            PLit (Origin None src)
                <$> resolveSign sign
                <*> resolveLiteral lit
        PList src pats ->
            PList (Origin None src)
                <$> mapM resolvePat pats
        PInfixApp src a con b ->
            PInfixApp (Origin None src)
                <$> resolvePat a
                <*> resolveQName NsValues con
                <*> resolvePat b
        _ -> error $ "resolvePat: " ++ show pat

-- resolveGuardedAlts :: Resolve GuardedAlts
-- resolveGuardedAlts galts =
--     case galts of
--         UnGuardedAlt src expr ->
--             UnGuardedAlt (Origin None src)
--                 <$> resolveExp expr
--         _ -> error "resolveGuardedAlts"

resolveBinds :: Resolve Binds
resolveBinds binds =
    case binds of
        BDecls src decls ->
            BDecls (Origin None src)
                <$> mapM (resolveDecl ResolveToplevel) decls
        _ -> error "Language.Haskell.Scope.resolveBinds: undefined"

resolveAlt :: Resolve Alt
resolveAlt (Alt src pat rhs mbBinds) = do
    pat' <- resolvePat pat
    limitScope "branch" $
        Alt (Origin None src) pat'
            <$> resolveRhs rhs
            <*> resolveMaybe resolveBinds mbBinds

resolveQOp :: Resolve QOp
resolveQOp qop =
    case qop of
        QVarOp src qname ->
            QVarOp (Origin None src)
                <$> resolveQName NsValues qname
        QConOp src qname ->
            QConOp (Origin None src)
                <$> resolveQName NsValues qname

resolveLiteral :: Resolve Literal
resolveLiteral lit = pure $
    case lit of
        Char src c orig   -> worker Char src c orig
        String src s orig -> worker String src s orig
        Int src i orig    -> worker Int src i orig
        Frac src r orig   -> worker Frac src r orig
        PrimInt src i orig -> worker PrimInt src i orig
        PrimWord src w orig -> worker PrimWord src w orig
        PrimFloat src f orig -> worker PrimFloat src f orig
        PrimDouble src d orig -> worker PrimDouble src d orig
        PrimChar src c orig -> worker PrimChar src c orig
        PrimString src s orig -> worker PrimString src s orig
  where
    worker con src =
        con (Origin None src)

-- XXX: Support debindable syntax.
resolveStmt :: Resolve Stmt
resolveStmt stmt = do
    case stmt of
        Generator src pat expr ->
            Generator (Origin None src)
                <$> resolvePat pat
                <*> limitScope "gen" (resolveExp expr)
        Qualifier src expr ->
            Qualifier (Origin None src)
                <$> limitScope "qual" (resolveExp expr)
        _ -> error $ "resolveStmt: " ++ show stmt

resolveExp :: Resolve Exp
resolveExp expr =
    case expr of
        Case src scrut alts ->
            Case (Origin None src)
                <$> resolveExp scrut
                <*> mapMWithLimit "alt" resolveAlt alts
        Con src qname ->
            Con (Origin None src)
                <$> resolveQName NsValues qname
        Var src qname ->
            Var (Origin None src)
                <$> resolveQName NsValues qname
        App src a b ->
            App (Origin None src)
                <$> resolveExp a
                <*> resolveExp b
        InfixApp src a qop b ->
            InfixApp (Origin None src)
                <$> resolveExp a
                <*> resolveQOp qop
                <*> resolveExp b
        Paren src sub ->
            Paren (Origin None src)
                <$> resolveExp sub
        Lambda src pats sub -> limitScope "lambda" $
            Lambda (Origin None src)
                <$> mapM resolvePat pats
                <*> resolveExp sub
        Lit src lit ->
            Lit (Origin None src)
                <$> resolveLiteral lit
        Tuple src boxed exps ->
            Tuple (Origin None src) boxed
                <$> mapMWithLimit "tuple" resolveExp exps
        Let src binds expr -> limitScope "let" $
            Let (Origin None src)
                <$> resolveBinds binds
                <*> resolveExp expr
        List src exprs ->
            List (Origin None src)
                <$> mapMWithLimit "list" resolveExp exprs
        Do src stmts ->
            Do (Origin None src)
                <$> mapM resolveStmt stmts
        _ -> error $ "resolveExp: " ++ show expr

resolveRhs :: Resolve Rhs
resolveRhs rhs =
    case rhs of
        UnGuardedRhs src expr ->
            UnGuardedRhs (Origin None src)
                <$> resolveExp expr
        _ -> error "resolveRhs"

resolveMatch :: Name Origin -> Resolve Match
resolveMatch name match =
  case match of
    Match src _name pats rhs mbBinds -> do
      pats <- mapM resolvePat pats
      limitScope "rhs" $
        Match (Origin None src) name pats
          <$> resolveRhs rhs
          <*> resolveMaybe resolveBinds mbBinds
    InfixMatch src leftPat _name rightPats rhs mbBinds -> do
      leftPat' <- resolvePat leftPat
      rightPats' <- mapM resolvePat rightPats
      limitScope "rhs" $
        InfixMatch (Origin None src) leftPat' name rightPats'
          <$> resolveRhs rhs
          <*> resolveMaybe resolveBinds mbBinds

resolveClassDecl :: Resolve ClassDecl
resolveClassDecl decl =
    case decl of
        ClsDecl src sub ->
          ClsDecl (Origin None src)
            <$> resolveDecl ResolveClass sub
        _ -> error "resolveClassDecl"

resolveFunDep :: Resolve FunDep
resolveFunDep dependency =
    case dependency of
        _ -> error "resolveFunDep"

resolveInstDecl :: Resolve InstDecl
resolveInstDecl inst =
    case inst of
        InsDecl src decl ->
            InsDecl (Origin None src)
                <$> resolveDecl ResolveInstance decl
        _ -> error "resolveInstDecl"

resolveActivation :: Resolve Activation
resolveActivation activation = pure $
    case activation of
        ActiveFrom src n -> ActiveFrom (Origin None src) n
        ActiveUntil src n -> ActiveUntil (Origin None src) n

resolveAssoc :: Resolve Assoc
resolveAssoc assoc = pure $
    case assoc of
        AssocNone src -> AssocNone (Origin None src)
        AssocLeft src -> AssocLeft (Origin None src)
        AssocRight src -> AssocRight (Origin None src)

resolveOp :: Resolve Op
resolveOp op =
  case op of
    VarOp src name -> VarOp (Origin None src) <$> resolveName NsValues name
    ConOp src name -> ConOp (Origin None src) <$> resolveName NsValues name

resolveDecl :: ResolveContext -> Resolve Decl
resolveDecl rContext decl =
  case decl of
    DataDecl src isNewtype ctx dhead cons derive ->
      limitTyVarScope $
      DataDecl (Origin None src)
          <$> resolveDataOrNew isNewtype
          <*> resolveMaybe resolveContext ctx
          <*> resolveDeclHead dhead
          <*> mapM resolveQualConDecl cons
          <*> resolveMaybe resolveDeriving derive
    FunBind src matches -> do
        -- We use the bind site to uniquely identify
        -- top-level functions. For class declarations,
        -- we use their type signature.
        name <- case rContext of
                  ResolveToplevel -> defineName NsValues (matchName $ head matches)
                  _               -> resolveName NsValues (matchName $ head matches)
        FunBind (Origin None src)
            <$> mapMWithLimit "match" (resolveMatch name) matches
    -- FIXME: PatBind in classes and instances
    PatBind src pat rhs binds -> do
        pat' <- resolvePat pat
        limitScope "rhs" $ PatBind (Origin None src)
            <$> pure pat'
            <*> resolveRhs rhs
            <*> resolveMaybe resolveBinds binds


    -- There's an implicit forall here.
    -- Gather up all the unbound type variables and bind them
    -- to the point of the signature.
    TypeSig src names ty | rContext == ResolveToplevel -> do
        -- FIXME: Bind tyvars to the src of the definition, not the tysig.
        TypeSig (Origin None src)
            <$> mapM (resolveName NsValues) names
            <*> resolveType ty
    TypeSig src names ty | rContext == ResolveClass -> do
        TypeSig (Origin None src)
            <$> mapM (defineName NsValues) names
            <*> resolveType ty
    ClassDecl src ctx dhead deps decls -> limitTyVarScope $
        ClassDecl (Origin None src)
            <$> resolveMaybe resolveContext ctx
            <*> resolveDeclHead dhead
            <*> mapM resolveFunDep deps
            <*> resolveMaybe (mapM resolveClassDecl) decls

    InstDecl src mbOverlap instRule decls -> limitTyVarScope $
        InstDecl (Origin None src)
            <$> resolveMaybe resolveOverlap mbOverlap
            <*> resolveInstRule instRule
            <*> resolveMaybe (mapM resolveInstDecl) decls

    ForImp src conv safety ident name ty ->
        -- Bind free type variables to the foreign import.
        ForImp (Origin None src)
            <$> resolveCallConv conv
            <*> resolveMaybe resolveSafety safety
            <*> pure ident
            <*> defineName NsValues name
            <*> resolveType ty
    InlineSig src noInline mbActivation qname ->
        InlineSig (Origin None src) noInline
            <$> resolveMaybe resolveActivation mbActivation
            <*> resolveQName NsValues qname

    TypeDecl src dhead ty ->
        TypeDecl (Origin None src)
            <$> resolveDeclHead dhead
            <*> resolveType ty

    InfixDecl src assoc precedence ops ->
        InfixDecl (Origin None src)
            <$> resolveAssoc assoc
            <*> pure precedence
            <*> mapM resolveOp ops

    _ -> error $ "resolveDecl: " ++ show decl

resolveModuleName :: Resolve ModuleName
resolveModuleName (ModuleName src name) = do
    pure $ ModuleName (Origin None src) name

resolveNamespace :: Resolve Namespace
resolveNamespace ns =
    case ns of
        NoNamespace src ->
            pure $ NoNamespace (Origin None src)
        TypeNamespace src ->
            pure $ TypeNamespace (Origin None src)

resolveCName :: Resolve CName
resolveCName cname =
    case cname of
        ConName src name ->
            ConName (Origin None src)
                <$> resolveName NsValues name

resolveEWildcard :: Resolve EWildcard
resolveEWildcard wildcard =
  case wildcard of
    NoWildcard src -> pure $ NoWildcard (Origin None src)
    EWildcard src n -> pure $ EWildcard (Origin None src) n

resolveExportSpec :: Resolve ExportSpec
resolveExportSpec spec =
    case spec of
        EAbs src ns qname ->
            EAbs (Origin None src) <$> resolveNamespace ns <*> resolveQName NsTypes qname
        EVar src qname ->
            EVar (Origin None src)
                -- <$> resolveNamespace ns
                <$> resolveQName NsValues qname
        EThingWith src wild qname cnames ->
            EThingWith (Origin None src)
                <$> resolveEWildcard wild
                <*> resolveQName NsTypes qname
                <*> mapM resolveCName cnames
        _ -> error $ "resolveExportSpec: " ++ show spec

resolveExportSpecList :: Resolve ExportSpecList
resolveExportSpecList list =
    case list of
        ExportSpecList src exports ->
            ExportSpecList (Origin None src)
                <$> mapM resolveExportSpec exports

resolveModuleHead :: Resolve ModuleHead
resolveModuleHead (ModuleHead src name mbWarn mbExport) =
    ModuleHead (Origin None src)
        <$> resolveModuleName name
        <*> resolveMaybe undefined mbWarn
        <*> resolveMaybe resolveExportSpecList mbExport

resolveModulePragma :: Resolve ModulePragma
resolveModulePragma pragma =
    case pragma of
        LanguagePragma src names ->
            pure $ LanguagePragma (Origin None src) (map scope names)
        _ -> error "resolveModulePragma"
  where
    scope (Ident src ident) = Ident (Origin None src) ident
    scope (Symbol src symbol) = Symbol (Origin None src) symbol

resolveImportSpec :: Resolve ImportSpec
resolveImportSpec spec =
    case spec of
        IVar src name ->
            IVar (Origin None src)
                <$> resolveName NsValues name
        _ -> error "Language.Haskell.Scope.resolveImportSpec"

resolveImportSpecList :: Resolve ImportSpecList
resolveImportSpecList (ImportSpecList src bool specs) =
    ImportSpecList (Origin None src) bool
        <$> mapM resolveImportSpec specs

bindImports :: ImportDecl Origin -> Rename ()
bindImports ImportDecl{..} = do
    iface <- getInterface modName
    addToScope (ModuleSource importModule) "" iface
  where
    modName = case importModule of ModuleName _ name -> name

resolveImportDecl :: Resolve ImportDecl
resolveImportDecl ImportDecl{..} = do
    decl <- ImportDecl (Origin None importAnn)
        <$> resolveModuleName importModule
        <*> pure importQualified
        <*> pure importSrc
        <*> pure importSafe
        <*> pure importPkg
        <*> resolveMaybe resolveModuleName importAs
        <*> resolveMaybe resolveImportSpecList importSpecs
    bindImports decl
    return decl

resolveModule :: Resolve Module
resolveModule m =
    case m of
        Module src mhead pragma imports decls ->
            Module (Origin None src)
                <$> resolveMaybe resolveModuleHead mhead
                <*> mapM resolveModulePragma pragma
                <*> mapM resolveImportDecl imports
                <*> mapM (resolveDecl ResolveToplevel) decls
        _ -> error "resolveModule"
