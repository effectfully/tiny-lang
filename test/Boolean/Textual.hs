module Boolean.Textual
    ( test_printerParserRoundtrip
    ) where

import           TinyLang.Boolean.Core
import           TinyLang.Boolean.Generator ()
import           TinyLang.Boolean.Parser
import           TinyLang.Boolean.Printer
import           TinyLang.Var

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

---------------------------------------------------------------------------
-- Since we've got a generator, let's use it to test the parser and printer.

-- We want to check that printing then parsing is the identity, but in
-- general it won't be because the Uniques in the variables will change.
-- Let's get round that by setting all the IDs to 0.
-- I'm sure there's a fancy Haskell way to do this, but it's not hard to
-- do it the old-fashioned way.

forgetID :: Var -> Var
forgetID v = Var (Unique 0) (_varName v)

forgetIDs :: Expr -> Expr
forgetIDs (EVal b)             = EVal b
forgetIDs (EVar v)             = EVar (forgetID v)
forgetIDs (EAppUnOp op e)      = EAppUnOp op (forgetIDs e)
forgetIDs (EAppBinOp op e1 e2) = EAppBinOp op (forgetIDs e1) (forgetIDs e2)
forgetIDs (EIf e e1 e2)        = EIf (forgetIDs e) (forgetIDs e1) (forgetIDs e2)

checkparse :: Expr -> Bool
checkparse e =
    case parseExpr (toStringNoIDs e) of
        Left _  -> False
        Right f -> forgetIDs f == forgetIDs e

test_printerParserRoundtrip :: TestTree
test_printerParserRoundtrip =
    testProperty "printerParserRoundtrip" $
        withMaxSuccess 1000 $ property checkparse
