{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Scope.SyntaxDirected
    ( resolveModule
    ) where

import           Control.Monad.Reader
import           Control.Monad.Writer         (tell)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Monoid                  (Monoid (..))
import           Language.Haskell.Exts.Syntax

import           Language.Haskell.Scope.Monad

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
                            Left (ENotInScope qname ns)
                        IsTv ->
                            Right (ScopedName LocalSource (GlobalName loc qname))
                Just [var] -> Right var
                Just vars  -> Left (EAmbiguous vars)
            nameInfo =
                case ret of
                    Left err -> ScopeError err
                    Right (ScopedName _ gname) -> Resolved gname
        tell mempty{ scopeErrors =
              case ret of
                Left err -> [(src, err)]
                Right{}  -> [] }
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
            -- let Origin origin _ = ann name'
            UnQual (Origin None src)
                <$> pure name'
        Special src specialCon ->
            Special (Origin None src)
                <$> resolveSpecialCon specialCon

defineName :: RNamespace -> Resolve Name
defineName ns name = do
    thisModule <- asks readerModuleName
    loc <- getLocation
    let qname = QualifiedName "" ident
        gname = GlobalName loc (QualifiedName thisModule ident)
        resolved = ScopedName LocalSource gname
    case ns of
        NsValues ->
            tell mempty{ scopeValues = Map.singleton qname [resolved]}
        NsTypes  ->
            tell mempty{ scopeTypes = Map.singleton qname [resolved]}
        NsTypeVariables ->
            tell mempty{ scopeTyVars = Map.singleton qname [resolved] }
    return $ con (Origin (Binding gname) src) ident
  where
    (con, src, ident) =
      case name of
          Ident a b  -> (Ident, a, b)
          Symbol a b -> (Symbol, a, b)

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
        CxTuple src assts ->
            CxTuple (Origin None src)
                <$> mapM resolveAsst assts
        CxEmpty src -> pure (CxEmpty (Origin None src))
        _ -> error "resolveContext"

resolveOverlap :: Resolve Overlap
resolveOverlap _overlap = error "resolveOverlap"

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
        TyForall src mbTyVarBinds mbCtx ty' -> limitTyVarScope "forall" $
            TyForall (Origin None src)
                <$> resolveMaybe (mapM resolveTyVarBind) mbTyVarBinds
                <*> resolveMaybe resolveContext mbCtx
                <*> resolveType ty'
        TyList src ty' ->
            TyList (Origin None src)
                <$> resolveType ty'
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
                <$> mapMWithLocation "binds" (resolveDecl ResolveToplevel) decls
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
resolveStmt stmt =
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
        Let src binds inExpr -> limitScope "let" $
            Let (Origin None src)
                <$> resolveBinds binds
                <*> resolveExp inExpr
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

resolveMatch :: Resolve Match
resolveMatch match =
  case match of
    Match src name pats rhs mbBinds -> do
      name' <- resolveName NsValues name
      pats' <- mapM resolvePat pats
      limitScope "rhs" $
        Match (Origin None src) name' pats'
          <$> resolveRhs rhs
          <*> resolveMaybe resolveBinds mbBinds
    InfixMatch src leftPat name rightPats rhs mbBinds -> do
      name' <- resolveName NsValues name
      leftPat' <- resolvePat leftPat
      rightPats' <- mapM resolvePat rightPats
      limitScope "rhs" $
        InfixMatch (Origin None src) leftPat' name' rightPats'
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
resolveFunDep _dependency = error "resolveFunDep"

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
      limitTyVarScope "data" $
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
        let Origin gname _ = ann name
        FunBind (Origin gname src)
            <$> mapMWithLimit "match" resolveMatch matches
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
    TypeSig src names ty | rContext == ResolveToplevel ->
        -- FIXME: Bind tyvars to the src of the definition, not the tysig.
        TypeSig (Origin None src)
            <$> mapM (resolveName NsValues) names
            <*> resolveType ty
    TypeSig src names ty | rContext == ResolveClass ->
        TypeSig (Origin None src)
            <$> mapM (defineName NsValues) names
            <*> resolveType ty
    ClassDecl src ctx dhead deps decls -> limitTyVarScope "class" $
        ClassDecl (Origin None src)
            <$> resolveMaybe resolveContext ctx
            <*> resolveDeclHead dhead
            <*> mapM resolveFunDep deps
            <*> resolveMaybe (mapM resolveClassDecl) decls

    InstDecl src mbOverlap instRule decls -> limitTyVarScope "instance" $
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
resolveModuleName (ModuleName src name) =
    pure $ ModuleName (Origin None src) name

resolveNamespace :: Resolve Namespace
resolveNamespace ns = pure $
    case ns of
      NoNamespace src      -> NoNamespace (Origin None src)
      TypeNamespace src    -> TypeNamespace (Origin None src)
      PatternNamespace src -> PatternNamespace (Origin None src)

resolveCName :: Resolve CName
resolveCName cname =
    case cname of
      ConName src name ->
        ConName (Origin None src) <$> resolveName NsValues name
      VarName src name ->
        VarName (Origin None src) <$> resolveName NsValues name

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
                <*> mapMWithLocation "decl" (resolveDecl ResolveToplevel) decls
        _ -> error "resolveModule"
