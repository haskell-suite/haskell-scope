data Ty a = Case1 a | Case2 a

pat = case pat of
        Case1 a -> a
        Case2 a -> a
