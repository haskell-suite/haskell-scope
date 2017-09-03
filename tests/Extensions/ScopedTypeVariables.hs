{-# LANGUAGE ScopedTypeVariables #-}

fn :: forall a. () -> a
fn _ = worker False
  where
    worker :: Bool -> a
    worker = worker
