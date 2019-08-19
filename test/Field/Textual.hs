{- A simple printer/parser test: generate a random expression and see if
   you get the same thing back (modulo uniques) when you convert it to
   a string and then parse it again.
-}

module Field.Textual
    ( test_checkparse
    ) where

import           TinyLang.Field.Core
import           TinyLang.Field.F17
import           TinyLang.Field.Parser
import           TinyLang.Field.Printer
import           TinyLang.Field.Generator ()
import           TinyLang.Field.ParsableField
import           TinyLang.Var

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

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

prop_Ftest :: forall f . (Eq f, Show f, ParsableField f) => SomeUniExpr f -> Bool
prop_Ftest (SomeUniExpr uni expr) =
    case parseExpr (exprToString NoIDs expr) of
        Left _                         -> False
        Right (SomeUniExpr uni' expr') ->
            withGeqUni uni uni' (forgetIDs expr' == forgetIDs expr) False

test_checkparse :: TestTree
test_checkparse =
    testProperty "printer-parser roundtrip" $
        withMaxSuccess 1000 . property $ prop_Ftest @F17
