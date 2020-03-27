{-| A simple set of tests for the raw parser
-}

module Field.Raw.Textual
  ( gen_test_parsing
  ) where

import Field.TestUtils

import Data.String
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

testDir :: FilePath
testDir = "test" </> "Field" </> "Raw" </> "golden"

genTest :: FilePath -> TestTree
genTest filePath = goldenVsString name golden action
    where name   = takeBaseName filePath
          golden = goldenFile filePath
          action = fromString <$> either id show <$> parseFilePath filePath

gen_test_parsing :: IO TestTree
gen_test_parsing =
    discoverTests "parsing" testDir genTest
    
