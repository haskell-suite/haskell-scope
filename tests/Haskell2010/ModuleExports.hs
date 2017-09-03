module ModuleExports
  ( eVar
  , eVar -- duplicate
  , EAbs
  , EThingWith(Some)
  , EAnotherThingWith(..)
  , EClass(exported)
  , module ModuleExports
  ) where

eVar = eVar

data EAbs

data EThingWith = Some | Other

data EAnotherThingWith = AnotherSome | AnotherOther

class EClass a where
  exported :: a
  notExported :: a
