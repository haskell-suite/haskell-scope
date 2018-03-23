-- 'fromJust' is defined in Stdlib. If it isn't listed in the import spec,
-- it shouldn't conflict with our local definition.
module ModuleRestrictedImport (fromJust) where

import Stdlib (Int)

fromJust _ = ()
