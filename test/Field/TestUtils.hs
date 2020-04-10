module Field.TestUtils
    ( testFiles
    , goldenFile
    , parseRational
    , parseFilePath
    , discoverTests
    ) where

import           TinyLang.Prelude

import           TinyLang.Field.Raw.Core   (RawExpr)
import           TinyLang.Field.Raw.Parser
import           TinyLang.ParseUtils

import           System.FilePath
import           System.FilePath.Glob
import           Test.Tasty
import           Test.Tasty.HUnit

testFileExt :: String
testFileExt = ".field"

testFiles :: FilePath -> IO [FilePath]
testFiles testDir = sort <$> globDir1 pat testDir
    where pat = compile $ "*" ++ testFileExt

goldenFile :: FilePath -> FilePath
goldenFile filePath = dropExtension filePath ++ ".golden"

parseRational :: String -> String -> Either String (RawExpr Rational)
parseRational fileName str = parseString pTop fileName str

parseFilePath :: FilePath -> IO (Either String (RawExpr Rational))
parseFilePath filePath =
    parseRational fileName <$> readFile filePath
    where
        fileName = takeFileName filePath

discoverTests :: String -> FilePath -> (FilePath -> TestTree) -> IO TestTree
discoverTests groupName testDir genTest = do
    files <- testFiles testDir
    let testFoundAnyFiles =
            testCase "test discovery" $
            not (null files) @? "didn't find any " ++ testFileExt ++  " files in " ++ testDir
    let testCases = testFoundAnyFiles : map genTest files
    pure $ testGroup groupName $ testCases
