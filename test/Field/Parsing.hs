{- A simple parser test
-}

module Field.Parsing
  ( gen_test_parsing
  ) where

import TinyLang.Prelude
import TinyLang.Field.Raw.Parser
import Test.Tasty
import Test.Tasty.Golden

import System.FilePath
import System.FilePath.Glob

gen_test_parsing :: IO TestTree
gen_test_parsing =
    do
        files <- testFiles
        pure $ testGroup "parsing" $ map mkGolden files

testDir :: FilePath
testDir = "test" </> "Field" </> "raw"

testFiles :: IO [FilePath]
testFiles = globDir1 pat testDir
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
