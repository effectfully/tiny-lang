{-| A simple set of tests for the raw parser
-}

module Field.Raw.Textual
  ( gen_test_parsing
  ) where

import TinyLang.Prelude

import TinyLang.ParseUtils
import TinyLang.Field.Raw.Parser

import Data.Bifunctor (second)
import Data.List (sort)
import System.FilePath
import System.FilePath.Glob
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit


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

parseRational :: String -> String -> String
parseRational fileName str = either id id $ second show result
    where
        result = parseString (pTop @Rational) fileName str

parseFilePath :: FilePath -> IO String
parseFilePath filePath =
    parseRational fileName <$> readFile filePath
    where
        fileName = takeFileName filePath

goldenFile :: FilePath -> FilePath
goldenFile filePath = dropExtension filePath ++ ".golden"

mkGolden :: FilePath -> TestTree
mkGolden filePath = goldenVsString name golden action
    where name   = takeBaseName filePath
          golden = goldenFile filePath
          action = fromString <$> parseFilePath filePath
