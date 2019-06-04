module Main where

import qualified Boolean.Textual as Boolean (test_checkparse)
import qualified Field.Textual   as Field (test_checkparse)

import           Test.Tasty

test_all :: TestTree
test_all =
    testGroup "all"
        [ Boolean.test_checkparse
        , Field.test_checkparse
        ]

main :: IO ()
main = defaultMain test_all
