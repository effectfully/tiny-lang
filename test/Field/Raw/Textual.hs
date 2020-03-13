{- A simple parser test
-}

module Field.Raw.Textual
  ( gen_test_parsing
  ) where

import Data.List (sort)
import TinyLang.Prelude
import TinyLang.Field.Raw.Parser
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import System.FilePath
import System.FilePath.Glob

testDir :: FilePath
testDir = "test" </> "Field" </> "Raw" </> "golden"

gen_test_parsing :: IO TestTree
gen_test_parsing =
    do
        files <- testFiles
        let checkFoundFiles =
                testCase "test dicovery" $
                not (null files) @? "didn't find any test cases in " ++ testDir
        let testCases = checkFoundFiles : map mkGolden files
        pure $ testGroup "parsing" $ testCases

-- sorted list of test files
testFiles :: IO [FilePath]
testFiles = sort <$> globDir1 pat testDir
    where pat = compile "*.field"

parseFilePath :: FilePath -> IO String
parseFilePath filePath =
    parseString fileName <$> readFile filePath
    where
        fileName = takeFileName filePath

goldenFile :: FilePath -> FilePath
goldenFile filePath = dropExtension filePath ++ ".golden"

mkGolden :: FilePath -> TestTree
mkGolden filePath = goldenVsString name golden action
    where name   = takeBaseName filePath
          golden = goldenFile filePath
          action = fromString <$> parseFilePath filePath
