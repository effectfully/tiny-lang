{-# LANGUAGE QuasiQuotes #-}

-- | Unit tests for
-- |
-- | * renaming
-- | * free variables

module Field.Renaming
    ( test_free_variables
    ) where

-- import           TinyLang.Prelude

import           TinyLang.Field.Typed.Core
-- NOTE:  Importing IsString Typed.Core
import           TinyLang.Field.Typed.Parser ()

import           Test.Tasty
import           Test.Tasty.HUnit

test_free_variables :: TestTree
test_free_variables = testGroup "free variables"
    [   testGroup "let binding"
        [ testCase "should not bind variable before" $
            assertNonEmptyEnv $ freeVars "ext x; assert x == 1; let x = 1;"
        , testCase "should not bind variable in the definition" $
            assertNonEmptyEnv $ freeVars "ext x; let x = x;"
        , testCase "should bind its variable after" $
            emptyEnv @=? freeVars "let x = 1; assert x == 1;"
        ]
    ,   testGroup "assert statement"
        [ testCase "should make variable free" $
            assertNonEmptyEnv $ freeVars "ext ?x; assert ?x;"
        ]
    ,   testGroup "for loop"
        [ testCase "should not bind variable before" $
            assertNonEmptyEnv $ freeVars "ext x; assert x == 1; for x = 0 to 0 do end;"
        , testCase "shoud bind its variable in body" $
            emptyEnv @=? freeVars "for x = 0 to 0 do assert x == 0; end;"
        , testCase "should bind its variable after body" $
            emptyEnv @=? freeVars "ext x; for x = 0 to 0 do end; assert x == 1;"
        ]
    ] where
        freeVars = progFreeVarSigs @(AField Rational)
        emptyEnv = Env mempty
        assertNonEmptyEnv = assertBool "set of free vars should not be empty" . (emptyEnv /=)

-- -- TODO:  Add more tests
-- test_renaming :: TestTree
-- test_renaming = testGroup "renamings" []
