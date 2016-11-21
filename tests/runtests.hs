module Main (main) where

import           Control.Monad                  (fmap, mplus, when)
import           Data.Foldable                  (foldMap)
import           Data.List                      (intercalate, nub)
import           Language.Haskell.Exts          (ParseResult (..), SrcSpan (..),
                                                 SrcSpanInfo (..), parseFile)
import           Language.Haskell.Scope.Resolve
import           System.Directory               (doesFileExist)
import           System.Environment             (getArgs)
import           System.Exit                    (exitSuccess, exitFailure)
import           System.FilePath                (replaceExtension, (<.>))
import           System.IO                      (hPutStrLn, stderr)
import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import           Text.PrettyPrint.ANSI.Leijen   (Doc, indent, text, underline,
                                                 vsep, (<>))
import           Text.Printf                    (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      exist <- doesFileExist path
      when exist $ do
        info <- getScopeInfo path
        case info of
          Left err -> do
            hPutStrLn stderr err
            exitFailure
          Right msg -> do
            putStr msg
            exitSuccess
    _ -> return ()
  defaultMain unitTests

unitTests :: [Test]
unitTests =
  [ scopeTest "Basic"
  , scopeTest "Class1"
  , scopeTest "Class2"
  , scopeTest "Class3"
  , scopeTest "Instance1"
  , scopeTest "Instance2"
  , scopeTest "Instance3"

  , scopeTest "Shadowing1"
  , scopeTest "Shadowing2"
  , scopeTest "Shadowing3"
  , scopeTest "Shadowing4"
  , scopeTest "Shadowing5"
  , scopeTest "DataType1"
  , scopeTest "Types1"
  , scopeTest "Where1"
  , scopeTest "Where2"
  , scopeTest "Where3"
  , scopeTest "Where4"
  , scopeTest "Where5"
  , scopeTest "Where6"
  , scopeTest "Infix1"
  , scopeTest "BuiltIn1"
  , testGroup "Known issues"
    [ scopeTest "Error1"
    , scopeTest "Instance4"
    , scopeTest "Records1"
    , scopeTest "Records2" ]
  ]

scopeTest :: String -> Test
scopeTest name = testCase name $ do
  let testFile = name <.> "hs"
  expectedOutput <- readFile (replaceExtension testFile "stdout") `mplus` return ""
  output <- either id id `fmap` getScopeInfo testFile
  let isFailure = expectedOutput /= output
  when isFailure $ fail "Diff Error"

getScopeInfo :: FilePath -> IO (Either String String)
getScopeInfo file = do
  fileContent <- readFile file
  parsed <- parseFile file
  case parsed of
    ParseFailed position msg ->
      return $ Left $
        show position ++ "\n" ++
        msg
    ParseOk thisModule -> do
      let (_env, errs, scoped) = resolve emptyResolveEnv thisModule
          allResolved = nub $ foldMap getResolved scoped
          getResolved (Origin (Resolved gname) loc) = [(loc, gname)]
          getResolved _ = []
          bindings = nub $ foldMap getBinding scoped
          getBinding (Origin (Binding gname) loc) = [(loc, gname)]
          getBinding _ = []
          defIndex = zip (map snd bindings) [1::Int ..]
      return $ Right $ unlines $
        [ "Scope errors:" ] ++
        [ "  " ++ show (ppScopeError err) ++ "\n" ++
          show (ppLocation 4 fileContent loc)
        | (loc, err) <- errs ] ++
        [ "", "Definitions:" ] ++
        concat
        [ [ printf "  Definition %d:" (n::Int)
          , show $ ppLocation 4 fileContent loc ]
        | (loc, n) <- zip (map fst bindings) [1..] ] ++
        [ "", "Use sites:" ] ++
        concat
        [ [ printf "  Definition used: %s" (maybe (intercalate "." pos) show (lookup gname defIndex))
          , show $ ppLocation 4 fileContent usageLoc ]
        | (usageLoc, gname@(GlobalName pos _) ) <- allResolved ]

ppScopeError :: ScopeError -> Doc
ppScopeError err =
  case err of
    ENotInScope _ NsTypes -> text "Type not in scope."
    ENotInScope _ NsTypeVariables -> text "Type variable not in scope."
    ENotInScope _ NsValues -> text "Value not in scope."
    EAmbiguous _ambi -> text "Ambiguous."
    ETypeAsClass -> text "Type used as type class."
    ENotExported -> text "Identifier not exported from module."
    EModNotFound -> text "Unknown module."
    EInternal -> text "Internal error."


ppLocation :: Int -> String -> SrcSpanInfo -> Doc
ppLocation padding file srcSpanInfo =
    indent padding $ vsep $
    case relevantLines of
      [] -> []
      [line] ->
        let (before, line') = splitAt (beginColumn-1) line
            (highlight, after) = splitAt (endColumn-beginColumn) line'
        in [text before <> underline (text highlight) <> text after]
      (line:rest) -> map (underline . text) (line:rest)
  where
    relevantLines = take (endLine-beginLine+1) (drop (beginLine-1) (lines file))
    srcSpan = srcInfoSpan srcSpanInfo
    beginLine = srcSpanStartLine srcSpan
    beginColumn = srcSpanStartColumn srcSpan
    endLine = srcSpanEndLine srcSpan
    endColumn = srcSpanEndColumn srcSpan
