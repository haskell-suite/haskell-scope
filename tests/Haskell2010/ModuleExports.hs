module ModuleExports
  ( eVar
  , EAbs
  , EThingWith(Some)
  , EThingWith(..)
  , EClass(exported)
  , module ModuleExports
  ) where

eVar = eVar

data EAbs

data EThingWith = Some | Other

class EClass a where
  exported :: a
  notExported :: a
