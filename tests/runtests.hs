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
import           System.FilePath                 (replaceExtension, (<.>))
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
  [ scopeTest "Basic"
  , scopeTest "Class1"
  , scopeTest "Class2"
  , scopeTest "Class3"
  , scopeTest "Instance1"
  , scopeTest "Instance2"
  , scopeTest "Instance3"
  , scopeTest "Instance4"
  , scopeTest "Shadowing1"
  , scopeTest "Shadowing2"
  , scopeTest "Records1"
  , scopeTest "Records2"
  , scopeTest "DataType1"
  , scopeTest "Types1"
  , scopeTest "Where1"
  , scopeTest "Where2"
  , scopeTest "Where3"
  , scopeTest "Where4"
  , scopeTest "Where5"
  , scopeTest "Where6"
  , scopeTest "Error1"
  ]

--scopeTest :: String -> FilePath -> Test
scopeTest name = testCase name $ do
  let testFile = name <.> "hs"
  expectedOutput <- readFile (replaceExtension testFile "stdout") `mplus` return ""
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
          (_definitions, usage) = partition isDefinition allResolved
          definitions = nub [ definitionLoc | (usageLoc, GlobalName definitionLoc _) <- allResolved ]
          defIndex = zip definitions [1..]
      return $ Right $ unlines $
        [ "Scope errors:" ] ++
        [ "  " ++ show err | err <- errs ] ++
        [ "", "Definitions:" ] ++
        concat
        [ [ printf "  Definition %d:" (n::Int)
          , show $ ppLocation 4 fileContent def ]
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
      (line:rest) -> map (underline . text) (line:rest)
  where
    relevantLines = take (endLine-beginLine+1) (drop (beginLine-1) (lines file))
    srcSpan = srcInfoSpan srcSpanInfo
    beginLine = srcSpanStartLine srcSpan
    beginColumn = srcSpanStartColumn srcSpan
    endLine = srcSpanEndLine srcSpan
    endColumn = srcSpanEndColumn srcSpan
