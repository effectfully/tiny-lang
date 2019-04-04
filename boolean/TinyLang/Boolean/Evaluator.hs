module TinyLang.Boolean.Evaluator (evalExpr)
where

import           TinyLang.Boolean.Core
import           TinyLang.Boolean.Environment (Env, lookupVar)
import           TinyLang.Var

-- | A recursive evaluator for expressions. Perhaps simplistic, but it works.

evalExpr :: Env -> Expr -> Bool
evalExpr env (EVal b)      = b
evalExpr env (EVar v)      = lookupVar v env
evalExpr env (EIf e e1 e2) = if (evalExpr env e) then evalExpr env e1 else evalExpr env e2
evalExpr env (EAppUnOp op e) =
    case op of
      Not -> not (evalExpr env e)
evalExpr env (EAppBinOp op e1 e2) =
    case op of
      Or  -> (evalExpr env e1) || (evalExpr env e2)
      And -> (evalExpr env e1) && (evalExpr env e2)
      Xor -> (evalExpr env e1) /= (evalExpr env e2)


