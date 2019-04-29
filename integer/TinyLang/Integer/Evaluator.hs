module TinyLang.Integer.Evaluator
    ( TypedValue (..)
    , evalUnOp
    , evalBinOp
    , evalExpr
    ) where

import           TinyLang.Environment  (Env, lookupVar)
import           TinyLang.Integer.Core

evalUnOp :: UnOp a b -> a -> b
evalUnOp Not  = not
evalUnOp Neq0 = (/= 0)

evalBinOp :: BinOp a b c -> a -> b -> c
evalBinOp Or  = (||)
evalBinOp And = (&&)
evalBinOp Xor = (/=)
evalBinOp Add = (+)
evalBinOp Mul = (*)

data TypedValue = forall a. TypedValue (Universe a) a

-- Note that we could use dependent maps, but we don't.
-- | A recursive evaluator for expressions. Perhaps simplistic, but it works.
evalExpr :: Env TypedValue -> Expr a -> a
evalExpr _   (EVal _ bool) = bool
evalExpr env (EVar u var) = case lookupVar var env of
    TypedValue u' val -> withGeqUniverse u u' val $ error "type mismatch"
evalExpr env (EIf e e1 e2) = if evalExpr env e then evalExpr env e1 else evalExpr env e2
evalExpr env (EAppUnOp op e) = evalUnOp op (evalExpr env e)
evalExpr env (EAppBinOp op e1 e2) =
    evalBinOp op (evalExpr env e1) (evalExpr env e2)
