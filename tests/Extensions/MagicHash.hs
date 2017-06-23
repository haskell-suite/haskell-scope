{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE MagicHash #-}
module MagicHash where

primint = 1#
primword = 1##
primfloat = 1.0#
primdouble = 1.0##
primchar = 'c'#
primstring = "string"#

data Int#
data Int = I# Int#
