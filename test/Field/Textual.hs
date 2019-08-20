{- A simple printer/parser test: generate a random expression and see if
   you get the same thing back (modulo uniques) when you convert it to
   a string and then parse it again.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Field.Textual
    ( test_printerParserRoundtrip
    ) where

import           TinyLang.Field.Core
import           TinyLang.Field.F17
import           TinyLang.Field.Parser
import           TinyLang.Field.Printer
import           TinyLang.Field.Generator
import           TinyLang.Field.ParsableField
import           TinyLang.Var

import           Test.QuickCheck
import           Test.QuickCheck.Property as Prop
import           Test.Tasty
import           Test.Tasty.QuickCheck

-- TODO: move me somewhere else.
instance Testable (Either String ()) where
    property = property . \case
        Left err -> failed { Prop.reason = err }
        Right () -> succeeded

forgetID :: UniVar f a -> UniVar f a
forgetID (UniVar u v) = UniVar u $ Var (Unique 0) (_varName v)

forgetIDs :: Expr f a -> Expr f a
forgetIDs (EVal uval)          = EVal uval
forgetIDs (EVar uvar)          = EVar $ forgetID uvar
forgetIDs (EAppUnOp op e)      = EAppUnOp op (forgetIDs e)
forgetIDs (EAppBinOp op e1 e2) = EAppBinOp op (forgetIDs e1) (forgetIDs e2)
forgetIDs (EIf e e1 e2)        = EIf (forgetIDs e) (forgetIDs e1) (forgetIDs e2)
forgetIDs (ELet uvar d e)      = ELet (forgetID uvar) (forgetIDs d) (forgetIDs e)
forgetIDs (EConstr econstr e)  = case econstr of
    EConstrFEq lhs rhs -> EConstr (EConstrFEq (forgetIDs lhs) (forgetIDs rhs)) (forgetIDs e)

{- Call this with eg
       quickCheck (withMaxSuccess 1000 (prop_Ftest :: SomeUniExpr Rational -> Bool))
   or
       quickCheck (stdArgs {maxSuccess=500, maxSize=1000}) (prop_Ftest :: SomeUniExpr F17 -> Bool)
-}

prop_Ftest :: forall f . (Eq f, ParsableField f) => SomeUniExpr f -> Either String ()
prop_Ftest (SomeUniExpr uni expr) = do
    SomeUniExpr uni' expr' <- parseExpr $ exprToString NoIDs expr

    withGeqUni uni uni' (if forgetIDs expr' == forgetIDs expr then Right () else Left "value mismatch") $ Left "type mismatch"

data Binding f = forall a. Binding (UniVar f a) (Expr f a)

deriving instance Show f => Show (Binding f)

instance (Field f, Arbitrary f) => Arbitrary (Binding f) where
    arbitrary =
        withOneofUnis $ \(_ :: Uni f a) ->
            Binding <$> arbitrary @(UniVar f a) <*> arbitrary

prop_nestedELet
    :: forall f. (Eq f, ParsableField f)
    => [Binding f] -> SomeUniExpr f -> Either String ()
prop_nestedELet bindings body0 = prop_Ftest $ foldr bind body0 bindings where
    bind :: Binding f -> SomeUniExpr f -> SomeUniExpr f
    bind (Binding uniVar body) (SomeUniExpr uni expr) =
        SomeUniExpr uni $ ELet uniVar body expr

test_checkParseGeneric :: TestTree
test_checkParseGeneric =
    testProperty "checkParseGeneric" $
        withMaxSuccess 1000 . property $ prop_Ftest @F17

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









-- A couple of functions for checking the output of generators
nodes :: Expr f a -> Int
nodes (EVal _)            = 1
nodes (EVar _)            = 1
nodes (EAppUnOp _ e)      = 1 + nodes e
nodes (EAppBinOp _ e1 e2) = 1 + nodes e1 + nodes e2
nodes (EIf e e1 e2)       = 1 + nodes e  + nodes e1 + nodes e2
nodes (ELet _ e1 e2)      = 1 + nodes e1 + nodes e2
nodes (EConstr _ _)       = 1  -- FIX THIS

depth :: Expr f a -> Int
depth (EVal _)            = 1
depth (EVar _)            = 1
depth (EAppUnOp _ e)      = 1 + depth e
depth (EAppBinOp _ e1 e2) = 1 + max (depth e1) (depth e2)
depth (EIf e e1 e2)       = 1 + max (depth e)  (max (depth e1) (depth e2))
depth (ELet _ e1 e2)      = 1 + max (depth e1) (depth e2)
depth (EConstr _ _)       = 1 -- FIX THIS


{- A function to exercise the generator: 'testGen n size' generates n
   expressions of the given size and reports some simple statistics.
   Changing frequencies in the generators can affect the average size
   (eg, if you generate variables with a high frequency it'll tend to
   stop the generator quite quickly, making it difficult to get a deep
   AST), so it's worth checking what actually happens now and then. -}

testGen :: Int -> Int -> IO ()
testGen n size =
    let arb = arbitrary :: Gen (Expr Rational (AField Rational))
        -- ^ Just so that we can define the generator near the top.
        maxInt = maxBound :: Int
    in do
      putStrLn ""
      loop n arb maxInt 0 0 maxInt 0 0
    where
      loop k arb mind maxd sumd minn maxn !sumn =
          if k <= 0 then
              let meand = Prelude.div sumd n
                  meann = Prelude.div sumn n
              in do
                putStrLn $ "\nRequested size = " ++ show size
                putStrLn ""
                putStrLn $ "Minimum depth = " ++ show mind
                putStrLn $ "Maximum depth = " ++ show maxd
                putStrLn $ "Mean depth    = " ++ show meand
                putStrLn ""
                putStrLn $ "Minimum number of nodes = " ++ show minn
                putStrLn $ "Maximum number of nodes = " ++ show maxn
                putStrLn $ "Mean number of nodes    = " ++ show meann
                putStrLn ""
          else
              do
                putStr $ "Generated " ++ show (n-k+1) ++ " ASTs\r"
                e <- generate (resize size arb)
                let d = depth e
                    m = nodes e
                loop (k-1) arb (min mind d) (max maxd d) (sumd + d)
                               (min minn m) (max maxn m) (sumn + m)
