module Main where

import qualified Boolean.Textual   as Boolean (test_printerParserRoundtrip)
import qualified Field.Axioms      as Field (test_axiomsExamples)
import qualified Field.Textual     as Field (test_textual)
import qualified Field.Raw.Textual as Raw (gen_test_parsing)

import           Test.Tasty

test_all :: IO TestTree
test_all = do
    test_rawParsing <- Raw.gen_test_parsing
    pure $
        testGroup "all"
        [ Boolean.test_printerParserRoundtrip
        , Field.test_axiomsExamples
        , Field.test_textual
        , test_rawParsing
        ]

main :: IO ()
main =  defaultMain =<< test_all
