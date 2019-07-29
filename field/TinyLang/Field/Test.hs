{- A simple printer/parser test: generate a random expression and see if
   you get the same thing back (modulo uniques) when you convert it to
   a string and then parse it again.
-}

module TinyLang.Field.Test (prop_Ftest)
where

import           TinyLang.Field.Core
import           TinyLang.Field.Parser
import           TinyLang.Field.Printer
import           TinyLang.Field.Generator ()
import           TinyLang.Field.ParsableField
import           TinyLang.Var

forgetID :: Var -> Var
forgetID v = Var (Unique 0) (_varName v)

forgetIDs :: Expr f a -> Expr f a
forgetIDs (EVal b)             = EVal b
forgetIDs (EVar u v)           = EVar u (forgetID v)
forgetIDs (EAppUnOp op e)      = EAppUnOp op (forgetIDs e)
forgetIDs (EAppBinOp op e1 e2) = EAppBinOp op (forgetIDs e1) (forgetIDs e2)
forgetIDs (EIf e e1 e2)        = EIf (forgetIDs e) (forgetIDs e1) (forgetIDs e2)


{- Call this with eg
       quickCheck (withMaxSuccess 1000 (prop_Ftest :: SomeUniExpr Rational -> Bool))
   or  
       quickCheckWith (stdArgs {maxSuccess=500, maxSize=1000}) (prop_Ftest :: SomeUniExpr F17 -> Bool)
-}

prop_Ftest :: forall f . (Eq f, Show f, ParsableField f) => SomeUniExpr f -> Bool
prop_Ftest expr =
    case expr of
      SomeUniExpr Field e ->
          case parseExpr (exprToString NoIDs e) of
            Left _   -> False
            Right (expr'::SomeUniExpr f) ->  
                case expr' of
                  SomeUniExpr Field e' -> forgetIDs e' == forgetIDs e
                  _ -> False
          -- Without the type ascription on expr' you get errors about untouchable types.

      SomeUniExpr Bool e ->
          case parseExpr (exprToString NoIDs e) of
            Left _   -> False
            Right (expr'::SomeUniExpr f) ->
                case expr' of
                  SomeUniExpr Bool e' -> forgetIDs e' == forgetIDs e
                  _ -> False



