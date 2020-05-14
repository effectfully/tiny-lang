module Main where

import qualified Field.Axioms        as Field (test_fields)
import qualified Field.Raw.Textual   as Raw (gen_test_parsing)
import qualified Field.Renaming      as Field (test_renaming, test_free_variables)
import qualified Field.Textual       as Field (gen_test_roundtrip, test_textual)
import qualified Field.Typed.Textual as Field (gen_test_typechecking)

import           Test.Tasty

test_all :: IO TestTree
test_all =
    testGroup "all" <$> sequence
        [ pure Field.test_free_variables
        , pure Field.test_renaming
        -- Old tests
        , pure Field.test_fields
        , pure Field.test_textual
        , Field.gen_test_roundtrip
        , Raw.gen_test_parsing
        , Field.gen_test_typechecking
        ]


main :: IO ()
main = defaultMain =<< test_all
