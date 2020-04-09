{- A simple printer/parser test: generate a random expression and see if
   you get the same thing back (modulo uniques) when you convert it to
   a string and then parse it again.
-}

module Field.Textual
    ( test_textual
    ) where

import           Data.Field.F17
import           TinyLang.Field.Generator
import qualified TinyLang.Field.Jubjub       as JJ
import           TinyLang.Field.Printer
import           TinyLang.Field.Typed.Core
import           TinyLang.Field.Typed.Parser
import           TinyLang.Prelude

import           System.Directory
import           System.FilePath
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.QuickCheck

-- TODO: we shouldn't forget uniques, because we ignore name shadowing problems in
-- generators. I.e. we should implement alpha-equality (but it is kind of weird to change
-- uniques of free variables and so we probably want a newtype wrapper around @Expr@ with
-- that very specific @Eq@ instance).
forgetID :: UniVar f a -> UniVar f a
forgetID (UniVar u v) = UniVar u $ Var (Unique 0) (_varName v)

forgetStatementIDs :: Statement f -> Statement f
forgetStatementIDs (ELet uvar d)  = ELet (forgetID uvar) (forgetIDs d)
forgetStatementIDs (EAssert expr) = EAssert $ forgetIDs expr

forgetIDs :: Expr f a -> Expr f a
forgetIDs (EConst uval)        = EConst uval
forgetIDs (EVar uvar)          = EVar $ forgetID uvar
forgetIDs (EAppUnOp op e)      = EAppUnOp op (forgetIDs e)
forgetIDs (EAppBinOp op e1 e2) = EAppBinOp op (forgetIDs e1) (forgetIDs e2)
forgetIDs (EIf e e1 e2)        = EIf (forgetIDs e) (forgetIDs e1) (forgetIDs e2)
forgetIDs (EStatement stat e)  = EStatement (forgetStatementIDs stat) (forgetIDs e)

{- Call this with eg
       quickCheck (withMaxSuccess 1000 (prop_Ftest :: SomeUniExpr Rational -> Bool))
   or
       quickCheck (stdArgs {maxSuccess=500, maxSize=1000}) (prop_Ftest :: SomeUniExpr F17 -> Bool)
-}

prop_Ftest :: (Eq f, TextField f) => SomeUniExpr f -> Either String ()
prop_Ftest (SomeOf uni expr) = do
    SomeOf uni' expr' <- runSupplyT $ parseExpr (exprToString NoIDs expr)
    let checkResult expr'' =
            when (forgetIDs expr /= forgetIDs expr'') . Left $ concat
                [ exprToString NoIDs expr
                , " is not equal to "
                , exprToString NoIDs expr'
                ]
        uniMismatch =
            Left $ concat
                [ show uni
                , " is not equal to "
                , show uni'
                ]
    withGeqUni uni uni' uniMismatch $ checkResult expr'

data Binding f = forall a. Binding (UniVar f a) (Expr f a)

deriving instance TextField f => Show (Binding f)

instance (Field f, Arbitrary f) => Arbitrary (Binding f) where
    arbitrary =
        withOneofUnis $ \(_ :: Uni f a) ->
            Binding @f @a . unDefaultUniVar <$> arbitrary <*> arbitrary

prop_nestedELet
    :: forall f. (Eq f, TextField f)
    => [Binding f] -> SomeUniExpr f -> Either String ()
prop_nestedELet bindings body0 = prop_Ftest $ foldr bind body0 bindings where
    bind :: Binding f -> SomeUniExpr f -> SomeUniExpr f
    bind (Binding uniVar body) (SomeOf uni expr) =
        SomeOf uni $ EStatement (ELet uniVar body) expr

test_checkParseGeneric :: TestTree
test_checkParseGeneric =
    testProperty "checkParseGeneric2" $
        withMaxSuccess 1000 . property $ prop_Ftest @JJ.F

test_checkParseNestedLets :: TestTree
test_checkParseNestedLets =
    testProperty "checkParseNestedLets" $
        withMaxSuccess 100 . property $ prop_nestedELet @F17

test_printerParserRoundtrip :: TestTree
test_printerParserRoundtrip =
    testGroup "printerParserRoundtrip"
        [ test_checkParseGeneric
        , test_checkParseNestedLets
        ]

parsePrint :: String -> String
parsePrint
    = either id (forget $ exprToString WithIDs)
    . runSupplyT
    . parseExpr @Rational

parsePrintGolden :: String -> String -> TestTree
parsePrintGolden name expr =
    withResource (createDirectoryIfMissing True folder) mempty $ \_ ->
        goldenVsString
            name
            (folder </> name <> ".golden")
            (return . fromString $ parsePrint expr)
  where
    folder = "test" </> "Field" </> "golden"

test_forLoops :: TestTree
test_forLoops = parsePrintGolden "forLoops" $ unlines
    [ "for i = 1 to 2 do"
    , "    let i' = i;"
    , "    for j = 2 to 3 do"
    , "        let k = i * j;"
    , "        assert k == i' * j;"
    , "    end;"
    , "    let p = i;"
    , "    for l = 1 to 2 do"
    , "        let p = p * l;"
    , "    end;"
    , "end;"
    , "p"
    ]

test_parsePrintGolden :: TestTree
test_parsePrintGolden =
    testGroup "parsePrintGolden"
        [ test_forLoops
        ]

test_textual :: TestTree
test_textual =
    testGroup "textual"
        [ test_printerParserRoundtrip
        , test_parsePrintGolden
        ]
