{-# LANGUAGE Haskell2010 #-}
module Pat where

{-
data Pat l
  = PVar l (Name l)
  | PLit l (Sign l) (Literal l)
  | PNPlusK l (Name l) Integer
  | PInfixApp l (Pat l) (QName l) (Pat l)
  | PApp l (QName l) [Pat l]
  | PTuple l Boxed [Pat l]
  | PList l [Pat l]
  | PParen l (Pat l)
  | PRec l (QName l) [PatField l]
  | PAsPat l (Name l) (Pat l)
  | PWildCard l
  | PIrrPat l (Pat l)
  | PatTypeSig l (Pat l) (Type l)
  | PViewPat l (Exp l) (Pat l)
  | PRPat l [RPat l]
  | PXTag l (XName l) [PXAttr l] (Maybe (Pat l)) [Pat l]
  | PXETag l (XName l) [PXAttr l] (Maybe (Pat l))
  | PXPcdata l String
  | PXPatTag l (Pat l)
  | PXRPats l [RPat l]
  | PQuasiQuote l String String
  | PBangPat l (Pat l)
-}

data List a = Nil | Cons a (List a)

data Field = Field { field1 :: (), field2 :: () }

pvar v = v

plit 0 = 0
plit (-1) = -0
plit 1 = 1

pinfix1 (x `Cons` xs) = x : xs
pinfix2 (x : xs) = x `Cons` xs

papp1 (Cons x xs) = x : xs
papp2 ((:) x xs) = x : xs

ptuple (a,b,c) = [a,b,c]

plist [a,b,c] = (a,b,c)

pparen ([a,b,c]) = (a,b,c)

prec Field{field1=f} = f
-- prec Field{field1} = field1
-- prec Field{..} = (field1, field2)

paspat lst@(x:xs) = (lst,x,xs)

pwildcard _ = ()

pirrpat ~(x:xs) = (x,xs)

-- ptypesig (x::()) = x

-- pviewpat

-- pbang1 !x = x
-- pbang2 (!x,!y) = [x,y]
