{- A simple printer/parser test: generate a random expression and see if
   you get the same thing back (modulo uniques) when you convert it to
   a string and then parse it again.
-}

module Field.Textual
    ( test_textual
    , gen_test_roundtrip
    ) where

import           Field.TestUtils
import           Data.Field.F17
import           TinyLang.Field.Generator    ()
import qualified TinyLang.Field.Jubjub       as JJ
import           TinyLang.Field.Printer
import           TinyLang.Field.Typed.Core
import           TinyLang.Field.Typed.Parser
import           TinyLang.Prelude
import           TinyLang.Field.Rename
import           TinyLang.Field.Evaluator

import           System.FilePath
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.QuickCheck


{- Call this with eg
       quickCheck (withMaxSuccess 1000 (prop_Ftest :: SomeUniExpr Rational -> Bool))
   or
       quickCheck (stdArgs {maxSuccess=500, maxSize=1000}) (prop_Ftest :: SomeUniExpr F17 -> Bool)
-}

-- Alpha equivalence for Programs
-- NOTE:  Var/UniVar/SomeUniVar are the same as long as unique is the same
aEquiv :: (Eq f) => Program f -> Program f -> Bool
aEquiv a b = fresh a == fresh b where
    fresh = runSupply . renameProgram


prop_prog_roundtrip :: forall f. (Eq f, TextField f) => Program f -> Either String ()
prop_prog_roundtrip prog = do
    prog' <- runSupplyT $ parseProgram @f $ progToString WithIDs prog
    unless (prog `aEquiv` prog') . Left $ concat
        [ progToString WithIDs prog
        , "is not equal to \n"
        , progToString WithIDs prog'
        , "\n\n"
        ]

data Binding f = forall a. Binding (UniVar f a) (Expr f a)

deriving instance TextField f => Show (Binding f)

-- instance (Field f, Arbitrary f) => Arbitrary (Binding f) where
--     arbitrary =
--         withOneOfUnis $ \(_ :: Uni f a) ->
--             Binding @f @a . unDefault <$> arbitrary <*> arbitrary

-- prop_nested_let
--     :: forall f. (Eq f, TextField f)
--     => [Binding f] -> Either String ()
-- prop_nested_let bindings = prop_prog_roundtrip $ Program mempty $ Statements $ map bind bindings where
--     bind :: Binding f -> Statement f
--     bind (Binding uniVar body) = ELet uniVar body

test_checkParseGeneric :: TestTree
test_checkParseGeneric =
    testProperty "checkParseGeneric2" $
        withMaxSuccess 1000 . property $ prop_prog_roundtrip @JJ.F

-- test_checkParseNestedLets :: TestTree
-- test_checkParseNestedLets =
--     testProperty "checkParseNestedLets" $
--         withMaxSuccess 100 . property $ prop_nested_let @F17

test_printerParserRoundtrip :: TestTree
test_printerParserRoundtrip =
    testGroup "printerParserRoundtrip"
        [ test_checkParseGeneric
        -- , test_checkParseNestedLets
        ]

test_textual :: TestTree
test_textual =
    testGroup "textual"
        [ test_printerParserRoundtrip
        , test_renaming
        ]

parsePrintFilePath :: FilePath -> IO String
parsePrintFilePath filePath = either id (progToString WithIDs) <$> typeCheckFilePath filePath

testDir :: FilePath
testDir = "test" </> "Field" </> "golden"

genTest :: FilePath -> TestTree
genTest filePath = goldenVsString name golden action
    where name   = takeBaseName filePath
          golden = goldenFile filePath
          action = fromString <$> parsePrintFilePath filePath

gen_test_roundtrip :: IO TestTree
gen_test_roundtrip =
    discoverTests "roundtrip golden" testDir genTest


prop_rename_same :: forall f. (TextField f)
    => Program f -> Either String ()
prop_rename_same prog =
    unless (norm' == norm) . Left $ unlines [ "renamed program"
                                          , norm'
                                          , "is different from the original program"
                                          , norm
                                          ] where
        prog' = runSupply $ renameProgram prog
        norm  = progToString NoIDs $ prog
        norm' = progToString NoIDs $ prog'


prop_rename_same_norm :: forall f. (Eq f, TextField f, AsInteger f)
    => ProgramWithEnv f -> Either String ()
prop_rename_same_norm (ProgramWithEnv prog env) =
    unless (result' == result) . Left $ unlines message where
        message = [ "ERROR"
                  , ind $ "normalisation of the renamed program did not match the normalisation of the original program"
                  , "INITIAL STATE"
                  , ind $ show env
                  , "ORIGINAL PROGRAM"
                  , ind $ progToString WithIDs prog
                  , "ORIGINAL PROGRAM NORMALISED"
                  , ind $ either show (progToString WithIDs) norm
                  , "RENAMED PROGRAM"
                  , ind $ progToString WithIDs prog'
                  , "RENAMED PROGRAM NORMALISED"
                  , ind $ either show (progToString WithIDs) norm'
                  ]
        ind     = indent "  "
        prog'   = runSupply $ renameProgram prog
        nrmProg = flip evalEval env . normProgram
        norm    = nrmProg prog
        norm'   = nrmProg prog'
        result  = either firstWord (progToString NoIDs) norm
        result' = either firstWord (progToString NoIDs) norm'

indent :: String -> String -> String
indent prefix = unlines . map (prefix++) . lines

firstWord :: (Show a) => a -> String
firstWord = takeWhile (not . isSpace) . show

prop_rename_same_eval :: forall f. (Eq f, TextField f, AsInteger f)
    => ProgramWithEnv f -> Either String ()
prop_rename_same_eval (ProgramWithEnv prog env) =
    unless (result' == result) . Left $ unlines message where
        message = [ "ERROR"
                  , ind $ "evaluation of the renamed program did not match the evaluation of the original program"
                  , "INITIAL STATE"
                  , ind $ show env
                  , "ORIGINAL PROGRAM"
                  , ind $ progToString WithIDs prog
                  , "ORIGINAL PROGRAM FINAL STATE"
                  , ind $ show result
                  , "RENAMED PROGRAM"
                  , ind $ progToString WithIDs prog'
                  , "RENAMED PROGRAM FINAL STATE"
                  , ind $ show result'
                  ]
        ind     = indent "  "
        prog'   = runSupply $ renameProgram prog
        eval    = flip execEval env . evalProgram
        -- TODO:  Fix equaity for errors
        result  = either firstWord show $ eval prog
        result' = either firstWord show $ eval prog'

test_renaming :: TestTree
test_renaming =
    testGroup "renaming" [ testProperty "is stable with respect to equality" $
                             withMaxSuccess 10000 . property $ prop_rename_same @F17
                         , testProperty "is stable with respect to normalisation" $
                             withMaxSuccess 1000 . property $ prop_rename_same_norm @F17
                         , testProperty "is stable with respect to evaluation" $
                             withMaxSuccess 1000 . property $ prop_rename_same_eval @F17
                         ]
