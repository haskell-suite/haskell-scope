{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
module API where

import qualified Data.Map as Map
import Data.Maybe

import AST
import Types
import Monad

limitInner :: Rename a -> Rename a
limitInner action = Rename $ \scope loc !st ->
  case unRename action scope loc st of
    (# ret, st' #) -> (# ret, st'{stateInnerScope = stateInnerScope st} #)

innerBlock :: Rename a -> Rename a
innerBlock action = Rename $ \scope loc !st ->
  let newScope = scope `mappend` stateInnerScope st'
      !(ret, st') =
        case unRename action newScope loc st of
          (# ret, st'' #) -> (ret, st'')
  in (# ret, st'{stateInnerScope = stateInnerScope st} #)


modifyInner :: (Scope -> Scope) -> Rename ()
modifyInner fn = Rename $ \_ _ st ->
  (# (), st{ stateInnerScope = fn (stateInnerScope st) } #)

addToInner :: String -> ScopedName -> Rename ()
addToInner ident scopedName = modifyInner (addToScope ident scopedName)

addToOuter :: String -> ScopedName -> Rename ()
addToOuter ident scopedName = Rename $ \_ _ st ->
  (# (), st{ stateOuterScope = addToScope ident scopedName (stateOuterScope st) } #)

bindEntity :: Name () -> Rename (Name NameInfo, ScopedName)
bindEntity (Name _ ident) = do
  loc <- askLocation
  scope <- askScope
  let entity = Entity loc ident Value
      err = case Map.lookup ident (scopeValues scope) of
        Just [_] -> Nothing
        Just es  -> Just (EAmbiguous es)
        Nothing  -> Nothing
      !ret = Name (maybe (Binding entity) ScopeError err) ident
      lazyError = ELazy $ maybeToList err
      scopedName = ScopedName LocalSource entity
  tellError lazyError
  return (ret, scopedName)

bindOuter :: Resolve Name
bindOuter name@(Name _ ident) = do
  (ret, scopedName) <- bindEntity name
  addToOuter ident scopedName
  return ret


bindInner :: Resolve Name
bindInner name@(Name _ ident) = do
  (ret, scopedName) <- bindEntity name
  addToInner ident scopedName
  return ret

resolveName :: Resolve Name
resolveName (Name _ ident) = do
  scope <- askScope
  let nameInfo = case Map.lookup ident (scopeValues scope) of
                   Just [ScopedName _src e] -> Resolved e
                   Just es  -> ScopeError (EAmbiguous es)
                   Nothing  -> ScopeError (ENotInScope ident Value)
      lazyError = ELazy $ case nameInfo of
                            ScopeError e -> [e]
                            _            -> []
  tellError lazyError
  return $! Name nameInfo ident

resolveWildcard :: Resolve Name
resolveWildcard (Name _ ident) = do
  scope <- askScope
  let nameInfo = case Map.lookup ident (scopeValues scope) of
                   Just [ScopedName _src e] -> Resolved e
                   Just es  -> ScopeError (EAmbiguous es)
                   Nothing  -> ScopeError (ENotInScope ident Value)
      lazyError = ELazy $ case nameInfo of
                            ScopeError e -> [e]
                            _            -> []
  tellError lazyError
  modifyInner $ \scope -> case nameInfo of
    ScopeError _ -> scope
    Resolved e   -> addToScope "newField" (ScopedName LocalSource e) scope
  return $! Name nameInfo ident
