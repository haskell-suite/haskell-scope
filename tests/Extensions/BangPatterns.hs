{-# LANGUAGE BangPatterns #-}

pbang1 !x = x
pbang2 (!x,!y) = [x,y]
