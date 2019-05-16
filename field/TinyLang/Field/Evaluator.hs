module TinyLang.Field.Evaluator
    ( SomeUniVal (..)
    , evalUnOp
    , evalBinOp
    , evalExpr
    ) where

import           Prelude               hiding (div)
import           TinyLang.Environment  (Env, lookupVar)
import           TinyLang.Field.Core

evalUnOp :: (Eq f, Field f) => UnOp f a b -> a -> b
evalUnOp Not  = not
evalUnOp Neq0 = (/= zer)
evalUnOp Neg  = neg
evalUnOp Inv  = inv

evalBinOp :: (Eq f, Field f) => BinOp f a b c -> a -> b -> c
evalBinOp Or  = (||)
evalBinOp And = (&&)
evalBinOp Xor = (/=)
evalBinOp FEq = (==)
evalBinOp Add = add
evalBinOp Sub = sub
evalBinOp Mul = mul
evalBinOp Div = div

data SomeUniVal f = forall a. SomeUniVal (Uni f a) a

-- Note that we could use dependent maps, but we don't.
-- | A recursive evaluator for expressions. Perhaps simplistic, but it works.
evalExpr :: (Eq f, Show f, Field f) => Env (SomeUniVal f) -> Expr f a -> a
evalExpr _   (EVal (UniVal _ x)) = x
evalExpr env (EVar u var) = case lookupVar var env of
    SomeUniVal u' val -> withGeqUni u u' val $ error "type mismatch"
evalExpr env (EIf e e1 e2) = if evalExpr env e then evalExpr env e1 else evalExpr env e2
evalExpr env (EAppUnOp op e) = evalUnOp op (evalExpr env e)
evalExpr env (EAppBinOp op e1 e2) =
    evalBinOp op (evalExpr env e1) (evalExpr env e2)
