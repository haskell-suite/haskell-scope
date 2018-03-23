module SyntaxDirected where

import AST
import Monad
import API

-----------------------------------------------------------------------
-- Scoping AST


scopeDeclaration :: Resolve Declaration
scopeDeclaration decl =
  case decl of
    FunDecl name patterns expr -> limitInner $
      FunDecl
        <$> bindOuter name
        <*> mapM scopePattern patterns
        <*> innerBlock (scopeExpr expr)
    DataDecl name fields ->
      DataDecl
        <$> bindOuter name
        <*> mapM scopeCon fields

scopeCon :: Resolve ConDecl
scopeCon (ConDecl name field) =
  ConDecl
    <$> bindOuter name
    <*> mapM bindOuter field

scopePattern pattern =
  case pattern of
    PatVar name -> PatVar <$> bindInner name
    -- PatField (Name l) (Name l) (Name l) -- A{b=c}
    PatWildcard name ->
      PatWildcard <$> resolveWildcard name

scopeExpr expr =
  case expr of
    Variable name -> Variable <$> resolveName name
    Application a b -> Application <$> scopeExpr a <*> scopeExpr b

scopeModule :: Resolve Module
scopeModule (Module decls) = Module <$> mapM scopeDeclaration decls
