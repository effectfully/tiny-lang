module Main where

-- import qualified Boolean.Textual as Boolean (test_printerParserRoundtrip)
-- import qualified Field.Axioms    as Field (test_axiomsExamples)
-- import qualified Field.Textual   as Field (test_textual)
import qualified Field.Parsing   as Parsing (test_parsing)

import           Test.Tasty

-- test_all :: TestTree
-- test_all =
--     testGroup "all"
--         [ Boolean.test_printerParserRoundtrip
--         , Field.test_axiomsExamples
--         , Field.test_textual
--         ]

test_all :: TestTree
test_all =
    testGroup "all"
    [ Parsing.test_parsing
    ]

main :: IO ()
main = defaultMain test_all
