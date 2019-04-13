module Main (main) where

import           Control.Monad                (fmap, unless, when)
import qualified Data.ByteString.Lazy         as BL
import           Data.Foldable                (foldMap)
import           Data.List                    (intercalate, sort)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Language.Haskell.Exts        (ParseResult (..), SrcSpan (..),
                                               SrcSpanInfo (..), parseFile)
import           Language.Haskell.Scope
import           System.Directory             (doesFileExist)
import           System.Environment           (getArgs)
import           System.Exit                  (exitFailure, exitSuccess)
import           System.FilePath              (replaceExtension, takeBaseName)
import           System.IO                    (hPutStrLn, stderr)
import           Text.PrettyPrint.ANSI.Leijen (Doc, indent, text, underline,
                                               vsep, (<$$>), (<>))
import           Text.Printf                  (printf)

import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.Golden

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
            putStr err
            hPutStrLn stderr ""
            exitFailure
          Right msg -> do
            putStr msg
            exitSuccess
    _ -> return ()
  goldenFiles <- sort <$> findByExtension [".stdout"] "tests"
  defaultMain $ testGroup "Tests"
    [ (if testName `elem` ignoreList
        then ignoreTest
        else id)
      (goldenVsText testName goldenFile (getScopeInfo' testFile))
    | goldenFile <- goldenFiles
    , let testFile = replaceExtension goldenFile "hs"
    , let testName = takeBaseName goldenFile
    ]
  where
    ignoreList = []

loadStdlib :: FilePath -> ResolveEnv -> IO ResolveEnv
loadStdlib path env = do
  fileContent <- readFile path
  parsed <- parseFile path
  case parsed of
    ParseFailed position msg ->
      fail $ show position ++ "\n" ++ msg
    ParseOk thisModule -> do
      let (env', errs, _scoped) = resolve env thisModule
      unless (null errs) $
        fail $ unlines $
          [ "Scope errors:" ] ++
          [ show (indent 2 $ ppScopeError err fileContent)
          | err <- errs ]
      return env'

getScopeInfo' :: FilePath -> IO String
getScopeInfo' path = fmap (either id id) (getScopeInfo path)

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
      initEnv <- loadStdlib "tests/Otherlib.hs" =<<
                 loadStdlib "tests/Stdlib.hs" emptyResolveEnv
      let (env, errs, scoped) = resolve initEnv thisModule
          allResolved = foldMap getResolved scoped
          getResolved (Origin (Resolved gname) loc) = [(loc, gname)]
          getResolved _                             = []
          bindings = foldMap getBinding scoped
          getBinding (Origin (Binding gname) loc) = [(loc, gname)]
          getBinding _                            = []
          defIndex = zip (map snd bindings) [1::Int ..]
          Just iface = lookupInterface (getModuleName thisModule) env
      return $ Right $ unlines $
        [ "Scope errors:" ] ++
        [ show (indent 2 $ ppScopeError err fileContent)
        | err <- errs ] ++
        [ "", "Definitions:" ] ++
        concat
        [ [ printf "  Definition %d, from %s:" (n::Int) (intercalate "." $ reverse addr)
          , show $ ppLocation 4 fileContent loc ]
        | ((loc, Entity addr _src _ _), n) <- zip bindings [1..] ] ++
        [ "", "Use sites:" ] ++
        concat
        [ [ printf "  Definition used: %s" (maybe (intercalate "." $ reverse pos) show (lookup entity defIndex))
          , show $ ppLocation 4 fileContent usageLoc ]
        | (usageLoc, entity@(Entity pos _src _name _) ) <- allResolved ] ++
        [ "", "Exports:" ] ++
        [ "  " ++ ppEntity entity
        | entity <- iface
        ]

ppEntity :: Entity -> String
ppEntity (Entity loc _src (QualifiedName _module identifier) kind) =
  identifier ++ ", " ++ show kind ++ " from " ++ last loc

ppScopeError :: ScopeError -> String -> Doc
ppScopeError err fileContent =
  case err of
    ENotInScope loc _ eKind ->
      text (t ++ " not in scope:") <$$>
      ppLocation 2 fileContent loc
      where t = show eKind
    EAmbiguous loc ambi ->
      text "Ambiguous:" <$$>
      indent 2
        (text "This identifier:" <$$>
        ppLocation 2 fileContent loc <$$>
        text "Could refer to any of these:" <$$>
        vsep [ ppLocation 2 fileContent (entitySrcSpan entity)
             | ScopedName _source entity <- ambi])
    EConflicting dups ->
      text "Conflicting definitions:" <$$>
      vsep [ ppLocation 2 fileContent (entitySrcSpan entity)
           | ScopedName _source entity <- dups]
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

goldenVsText :: TestName -> FilePath -> IO String -> TestTree
goldenVsText name path gen =
    goldenVsStringDiff name (\ref new -> ["diff", ref, new]) path gen'
  where
    gen' = BL.fromStrict . T.encodeUtf8 . T.pack <$> gen
