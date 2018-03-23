{-# LANGUAGE OverloadedStrings #-}
module Tests where

import AST
import Monad
import Types
import SyntaxDirected

runTest :: Module () -> (Module NameInfo, [ScopeError], Scope)
runTest = runRename . scopeModule

ppTest :: Module () -> IO ()
ppTest m = do
    putStrLn "Module:"
    print m'
    putStrLn "Error:"
    mapM_ print errs
    putStrLn "Scope:"
    print scope
  where
    (m', errs, scope) = runTest m

-----------------------------------------------------------------------
-- Test cases

-- #1
-- data Data = Field {field}
-- fn Data{bad = name} = name

test1 = Module
  [DataDecl "Data" [ConDecl "Field" ["field"]]
  ,FunDecl "fn" [PatField "Data" "field" "f"] (Variable "f")]

-- #2
-- data Data = Field {field :: Data}
-- fn Data{..} = field

test2 = Module
  [DataDecl "Data" [ConDecl "Field" ["field"]]
  ,FunDecl "fn1" [PatWildcard "Data"] (Variable "newField")
  ,FunDecl "fn2" [PatWildcard "Data"] (Variable "newField")]


-- #3
-- data Data = Field {field :: Data}
-- fn dat = field dat

-- #4
-- fn a a = c

test4 :: Module ()
test4 = Module [FunDecl "fn" [PatVar "a", PatVar "b"] (Variable "b") ]
