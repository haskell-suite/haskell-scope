{-# LANGUAGE Haskell2010 #-}
module Exp where

data Bool = True | False
data Record = Record {field1 :: Bool, field2 :: ()}
data Int

var = var
con = True
litChar = 'c'
litString = "string"
litInt = 10
litFrac = 3.14159
infixApp a b = a + b
a + b = a `infixApp` b
prefixApp a b = (+) a b
app = app app
negApp = -negApp
lambda = \x -> x
letexpr = let x = x in x
ifexpr = if True then False else True
casexpr = case True of True -> False; False -> True
doexpr = do x <- var -- #1
            let x_ = x
            x <- var -- #2
            y <- var
            let z = x y
            x y z x_
tuple = (tuple, tuple)
list = [list, list]
paren = (paren)
recConstr = Record{ field1 = True, field2 = () }
recUpdate = recConstr{ field1 = False }
enumfrom = [1..]
enumfromto = [1..10]
enumfromthen = [1, 10 ..]
enumfromthento = [1, 10 .. 100]
listcomp = [ y | x <- enumfrom, True, con, let y = x+y, let y = x+y ]
explicitTypeSig = litInt :: Int
