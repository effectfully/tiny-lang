{-# LANGUAGE QuasiQuotes #-}

-- | Unit tests for
-- |
-- | * renaming
-- | * free variables

module Field.Renaming
    ( test_free_variables
    , test_renaming
    ) where

-- import           TinyLang.Prelude

import           TinyLang.Field.Typed.Core
import           TinyLang.Field.Typed.Parser
import           TinyLang.Field.Rename
import           TinyLang.Field.Printer

import qualified Data.String.Interpolate.IsString as QQ
import           Test.Tasty
import           Test.Tasty.HUnit

test_free_variables :: TestTree
test_free_variables = testGroup "free variables"
    [   testGroup "let binding"
        [ testCase "should not bind variable before" $
            assertNonEmptyEnv $ freeVars "assert x == 1; let x = 1;"
        , testCase "should not bind variable in the definition" $
            assertNonEmptyEnv $ freeVars "let x = x;"
        , testCase "shoud bind its variable after" $
            emptyEnv @=? freeVars "let x = 1; assert x == 1;"
        ]
    ,   testGroup "assert statement"
        [ testCase "should make variable free" $
            assertNonEmptyEnv $ freeVars "assert ?x;"
        ]
    ,   testGroup "for loop"
        [ testCase "should not bind variable before" $
            assertNonEmptyEnv $ freeVars "assert x == 1; for x = 0 to 0 do end;"
        , testCase "shoud bind its variable in body" $
            emptyEnv @=? freeVars "for x = 0 to 0 do assert x == 0; end;"
        , testCase "should bind its variable after body" $
            emptyEnv @=? freeVars "for x = 0 to 0 do end; assert x == 1;"
        ]
    ] where
        freeVars = progFreeVarSigs @(AField Rational)
        emptyEnv = Env mempty
        assertNonEmptyEnv = assertBool "set of free vars should not be empty" . (emptyEnv /=)

-- TODO:  Add more tests
test_renaming :: TestTree
test_renaming = testGroup "renamings" []
