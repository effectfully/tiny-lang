module Main where

import qualified Boolean.Textual     as Boolean (test_printerParserRoundtrip)
import qualified Field.Axioms        as Field (test_fields)
import qualified Field.Raw.Textual   as Raw (gen_test_parsing)
import qualified Field.Textual       as Field (test_textual)
import qualified Field.Typed.Textual as Field (gen_test_typechecking)

import           Test.Tasty

test_all :: IO TestTree
test_all = do
    testGroup "all" <$> sequence
        [ pure Boolean.test_printerParserRoundtrip
        , pure Field.test_fields
        , pure Field.test_textual
        , Raw.gen_test_parsing
        , Field.gen_test_typechecking
        ]

main :: IO ()
main = defaultMain =<< test_all
