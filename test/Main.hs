module Main where

import qualified Boolean.Textual as Boolean (test_printerParserRoundtrip)
import qualified Field.Axioms    as Field (test_axiomsExamples)
import qualified Field.Textual   as Field (test_textual)

import           Test.Tasty

test_all :: TestTree
test_all =
    testGroup "all"
        [ Boolean.test_printerParserRoundtrip
        , Field.test_axiomsExamples
        , Field.test_textual
        ]

main :: IO ()
main = defaultMain test_all
