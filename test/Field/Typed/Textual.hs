module Field.Typed.Textual
    ( gen_test_typechecking
    ) where

import           Field.TestUtils

import           TinyLang.Field.Printer (Pretty(..))

import           Data.String
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.Golden

testDir :: FilePath
testDir = "test" </> "Field" </> "Typed" </> "golden"

genTest :: FilePath -> TestTree
genTest filePath = goldenVsString name golden action
    where name = takeBaseName filePath
          golden = goldenFile filePath
          action = fromString <$> either id (show . Pretty) <$> typeCheckFilePath filePath

gen_test_typechecking :: IO TestTree
gen_test_typechecking =
    discoverTests "type checking" testDir genTest
