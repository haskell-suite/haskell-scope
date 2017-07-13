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
    , Entity(..)
    , EntityKind(..)
    , QualifiedName(..)
    , RNamespace(..)
    , Interface(..)
    , Source(..)
    , ScopeError(..)
    , resolve
    -- , globalNameSrcSpanInfo

    -- XXX: Dont export.
    , getNameIdentifier
    ) where

import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax

import           Language.Haskell.Scope.Monad
import           Language.Haskell.Scope.SyntaxDirected

deriveInterface :: Scope -> Module Origin -> Interface
deriveInterface scope m =
    case m of
        Module _ (Just (ModuleHead _ _ _ (Just (ExportSpecList _ exports)))) _ _ _ ->
            [ entity
            | EVar _ qname <- exports
            , Just entity  <- [qnameToEntity qname] ] ++
            [ entity
            | EThingWith _ _wild _qname cnames <- exports
            , ConName _ name <- cnames
            , Just entity <- [nameToEntity name] ] ++
            [ entity
            | EThingWith _ _wild qname _cnames <- exports
            , Just entity <- [qnameToEntity qname] ] ++
            [ entity
            | EAbs _ _ns qname <- exports
            , Just entity <- [qnameToEntity qname] ]
        Module _ _ _ _ _ -> -- We don't have a module head or an export spec list.
          localEntities scope
        _ -> error "Language.Haskell.Scope.deriveInterface: undefined"

-- Resolve all names in a module
resolve :: ResolveEnv -> Module SrcSpanInfo -> (ResolveEnv, [ScopeError], Module Origin)
resolve resolveEnv m =
    let (scope, errs, m') = runRename resolveEnv $ resolveModule m
        iface = deriveInterface scope m'
        name = getModuleName m
    in (addInterface name iface resolveEnv, errs, m')
