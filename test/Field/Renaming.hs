{-# LANGUAGE QuasiQuotes #-}

-- | Unit tests for
-- |
-- | * renaming
-- | * free variables

module Field.Renaming
    ( test_free_variables
    , test_renaming
    ) where

import           TinyLang.Prelude

import           Data.Field.F17
import           TinyLang.Field.Generator    ()
import           TinyLang.Field.Typed.Core
-- NOTE:  Importing IsString Typed.Core
import           TinyLang.Field.Typed.Parser ()
import           TinyLang.Field.Rename

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

test_free_variables :: TestTree
test_free_variables = testGroup "free variables"
    [   testGroup "let binding"
        [ testCase "should not bind variable before" $
            assertNonEmptyEnv $ freeVars "ext x : field; assert x == 1; let x : field = 1;"
        , testCase "should not bind variable in the definition" $
            assertNonEmptyEnv $ freeVars "ext x : field; let x : field = x;"
        , testCase "should bind its variable after" $
            emptyEnv @=? freeVars "let x : field = 1; assert x == 1;"
        ]
    ,   testGroup "assert statement"
        [ testCase "should make variable free" $
            assertNonEmptyEnv $ freeVars "ext x : bool; assert x;"
        ]
    ,   testGroup "for loop"
        [ testCase "should not bind variable before" $
            assertNonEmptyEnv $ freeVars "ext x : field; assert x == 1; for x = 0 to 0 do end;"
        , testCase "shoud bind its variable in body" $
            emptyEnv @=? freeVars "for x = 0 to 0 do assert x == 0; end;"
        , testCase "should bind its variable after body" $
            emptyEnv @=? freeVars "ext x : field; for x = 0 to 0 do end; assert x == 1;"
        ]
    ] where
        freeVars = progFreeVarSigs @(AField Rational)
        emptyEnv = Env mempty
        assertNonEmptyEnv = assertBool "set of free vars should not be empty" . (emptyEnv /=)

fresh :: Program f -> Program f
fresh = runSupply . renameProgram

offset :: Program f -> Program f
offset prog = runSupply $ do
    _ <- freshUnique
    _ <- freshUnique
    _ <- freshUnique
    renameProgram prog

prop_fresh_preserves_equality :: (Eq f, TextField f) => Program f -> Either String ()
prop_fresh_preserves_equality prog = unless (prog == progR) . Left $ unlines message where
    message = [ "ERROR: fresh program"
              , show progR
              , "is not equal to"
              , show prog
              ]
    progR = fresh prog

prop_offset_causes_inequality :: (Eq f, TextField f) => Program f -> Either String ()
prop_offset_causes_inequality prog = unless (prog /= progO) . Left $ unlines message where
    message = [ "ERROR: offset program"
              , show progO
              , "is equal to"
              , show prog
              ]
    progO = offset prog

prop_fresh_recovers_equality :: (Eq f, TextField f) => Program f -> Either String ()
prop_fresh_recovers_equality prog = unless (progF == progOF) . Left $ unlines message where
    message = [ "ERROR: fresh . offset $ program"
              , show progOF
              , "is equal to"
              , show prog
              ]
    progF   = fresh prog
    progOF  = fresh . offset $ prog


test_renaming :: TestTree
test_renaming = testGroup "renaming"
    [   testGroup "preserves"
        [ testProperty "AST equality" $
            withMaxSuccess 1000 . property $ prop_fresh_preserves_equality @F17
        , testProperty "AST (pseudo)-inequality" $
            withMaxSuccess 1000 . property $ prop_offset_causes_inequality @F17
        , testProperty "AST equality" $
            withMaxSuccess 1000 . property $ prop_fresh_recovers_equality @F17
        ]
    ]
