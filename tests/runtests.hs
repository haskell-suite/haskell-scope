module Main (main) where

import           Control.Monad                   (fmap, mplus, when)
import           Data.Foldable                   (foldMap)
import           Data.List                       (nub, partition)
import           Data.Maybe                      (fromMaybe)
import           Language.Haskell.Exts.Annotated
import           Language.Haskell.Scope
import           System.Directory                (doesFileExist)
import           System.Environment              (getArgs)
import           System.Exit                     (ExitCode (..), exitWith)
import           System.FilePath                 (replaceExtension)
import           System.IO                       (hPutStrLn, stderr)
import           Test.Framework                  (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit
import           Text.PrettyPrint.ANSI.Leijen    (Doc, indent, text, underline,
                                                  vsep, (<>))
import           Text.Printf                     (printf)

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
            exitWith (ExitFailure 1)
          Right msg -> do
            putStr msg
            exitWith ExitSuccess
    _ -> return ()
  defaultMain unitTests

unitTests =
  [ scopeTest "Basic" "Basic.hs"
  , scopeTest "Class1" "Class1.hs"
  , scopeTest "Class2" "Class2.hs"
  , scopeTest "Shadowing1" "Shadowing1.hs"
  , scopeTest "Shadowing2" "Shadowing2.hs"
  , scopeTest "Records1" "Records1.hs"
  , scopeTest "Records2" "Records2.hs"
  , scopeTest "DataType1" "DataType1.hs"
  , scopeTest "Error1" "Error1.hs"
  ]

--scopeTest :: String -> FilePath -> Test
scopeTest name testFile = testCase name $ do
  expectedOutput <- readFile (replaceExtension testFile "expected.stdout") `mplus` return ""
  output <- either id id `fmap` getScopeInfo testFile
  when (expectedOutput /= output) $ do
    fail "Diff Error"

getScopeInfo :: FilePath -> IO (Either String String)
getScopeInfo file = do
  fileContent <- readFile file
  parsed <- parseFile file
  case parsed of
    ParseFailed position msg -> do
      return $ Left $
        show position ++ "\n" ++
        msg
    ParseOk thisModule -> do
      let (env, errs, scoped) = resolve emptyResolveEnv thisModule
          allResolved = nub $ foldMap getResolved scoped
          getResolved (Origin (Resolved gname) loc) = [(loc, gname)]
          getResolved _ = []
          isDefinition (usageLoc, GlobalName definitionLoc _)= usageLoc == definitionLoc
          (definitions, usage) = partition isDefinition allResolved
          defIndex = zip (map fst definitions) [1..]
      return $ Right $ unlines $
        [ "Scope errors:" ] ++
        [ "  " ++ show err | err <- errs ] ++
        [ "", "Definitions:" ] ++
        concat
        [ [ printf "  Definition %d:" (n::Int)
          , show $ ppLocation 4 fileContent (fst def) ]
        | (n, def) <- zip [1..] definitions ] ++
        [ "", "Use sites:" ] ++
        concat
        [ [ printf "  Definition used: %d" (fromMaybe 0 (lookup definitionLoc defIndex) :: Int)
          , show $ ppLocation 4 fileContent usageLoc ]
        | (usageLoc, GlobalName definitionLoc _) <- usage ]

ppLocation :: Int -> String -> SrcSpanInfo -> Doc
ppLocation padding file srcSpanInfo =
    indent padding $ vsep $
    case relevantLines of
      [] -> []
      [line] ->
        let (before, line') = splitAt (beginColumn-1) line
            (highlight, after) = splitAt (endColumn-beginColumn) line'
        in [text before <> underline (text highlight) <> text after]
      (line:rest) -> map text (line:rest)
  where
    relevantLines = take (endLine-beginLine+1) (drop (beginLine-1) (lines file))
    srcSpan = srcInfoSpan srcSpanInfo
    beginLine = srcSpanStartLine srcSpan
    beginColumn = srcSpanStartColumn srcSpan
    endLine = srcSpanEndLine srcSpan
    endColumn = srcSpanEndColumn srcSpan
