module Main where

import           Textual    (test_checkparse)

import           Test.Tasty

main :: IO ()
main = defaultMain test_checkparse
