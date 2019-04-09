module TinyLang.Boolean.Evaluator
    ( evalUnOp
    , evalBinOp
    , evalExpr
    ) where

import           TinyLang.Boolean.Core
import           TinyLang.Boolean.Environment (Env, lookupVar)

evalUnOp :: UnOp -> Bool -> Bool
evalUnOp Not = not

evalBinOp :: BinOp -> Bool -> Bool -> Bool
evalBinOp Or  = (||)
evalBinOp And = (&&)
evalBinOp Xor = (/=)

-- | A recursive evaluator for expressions. Perhaps simplistic, but it works.
evalExpr :: Env Bool -> Expr -> Bool
evalExpr _   (EVal b) = b
evalExpr env (EVar v) = lookupVar v env
evalExpr env (EIf e e1 e2) = if evalExpr env e then evalExpr env e1 else evalExpr env e2
evalExpr env (EAppUnOp op e) = evalUnOp op (evalExpr env e)
evalExpr env (EAppBinOp op e1 e2) =
    evalBinOp op (evalExpr env e1) (evalExpr env e2)
