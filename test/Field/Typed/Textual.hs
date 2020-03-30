module Field.Typed.Textual
    ( gen_test_typechecking
    ) where

import TinyLang.Var
import TinyLang.Field.Typed.Core (SomeUniExpr)
import TinyLang.Field.Typed.TypeChecker
import Field.TestUtils

import Data.String
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

testDir :: FilePath
testDir = "test" </> "Field" </> "Typed" </> "golden"

typeCheckFilePath :: FilePath -> IO (Either String (SomeUniExpr Rational))
typeCheckFilePath filePath = do
    parsed <- parseFilePath filePath
    pure $ runSupplyT . typeCheck =<< parsed
    
genTest :: FilePath -> TestTree
genTest filePath = goldenVsString name golden action
    where name = takeBaseName filePath
          golden = goldenFile filePath
          action = fromString <$> either id show <$> typeCheckFilePath filePath

gen_test_typechecking :: IO TestTree
gen_test_typechecking =
    discoverTests "type checking" testDir genTest
