-- Make sure importing the same identifiers many times doesn't cause problems.
module ModuleImportDuplicate where

import Stdlib
-- Interesting, importing the same identifier twice does not trigger a warning
-- in ghc-8.2.1, even with -Wall.
import Stdlib (Int, Int)

type MyInt = Int
