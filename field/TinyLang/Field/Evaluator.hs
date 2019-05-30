module TinyLang.Field.Evaluator
    ( evalUnOp
    , evalBinOp
    , evalExprUni
    , evalExpr
    , denoteUniVal
    , denoteExpr
    , normExpr
    ) where

import           Prelude              hiding (div)

import           TinyLang.Environment
import           TinyLang.Field.Core
import           TinyLang.Prelude

evalUnOp :: (Eq f, Field f) => UnOp f a b -> a -> UniVal f b
evalUnOp Not  = UniVal Bool . not
evalUnOp Neq0 = UniVal Bool . (/= zer)
evalUnOp Neg  = UniVal Field . neg
evalUnOp Inv  = UniVal Field . inv

evalBinOp :: (Eq f, Field f) => BinOp f a b c -> a -> b -> UniVal f c
evalBinOp Or  = UniVal Bool .* (||)
evalBinOp And = UniVal Bool .* (&&)
evalBinOp Xor = UniVal Bool .* (/=)
evalBinOp FEq = UniVal Bool .* (==)
evalBinOp Add = UniVal Field .* add
evalBinOp Sub = UniVal Field .* sub
evalBinOp Mul = UniVal Field .* mul
evalBinOp Div = UniVal Field .* div

-- Note that we could use dependent maps, but we don't.
-- | A recursive evaluator for expressions. Perhaps simplistic, but it works.
evalExprUni :: (Eq f, Field f) => Env (SomeUniVal f) -> Expr f a -> UniVal f a
evalExprUni _   (EVal uniVal) = uniVal
evalExprUni env (EVar u var) = case unsafeLookupVar var env of
    SomeUniVal uniVal@(UniVal u' _) -> withGeqUni u u' uniVal $ error "type mismatch"
evalExprUni env (EIf e e1 e2) = if evalExpr env e then evalExprUni env e1 else evalExprUni env e2
evalExprUni env (EAppUnOp op e) = evalUnOp op (evalExpr env e)
evalExprUni env (EAppBinOp op e1 e2) =
    evalBinOp op (evalExpr env e1) (evalExpr env e2)

-- | A recursive evaluator for expressions.
evalExpr :: (Eq f, Field f) => Env (SomeUniVal f) -> Expr f a -> a
evalExpr env = _uniValVal . evalExprUni env

denoteUniVal :: Field f => UniVal f a -> f
denoteUniVal (UniVal Bool  b) = if b then one else zer
denoteUniVal (UniVal Field i) = unAField i

denoteExpr :: (Eq f, Field f) => Env (SomeUniVal f) -> Expr f a -> f
denoteExpr env = denoteUniVal . evalExprUni env

-- | A recursive normalizer for expressions.
normExpr :: (Eq f, Field f) => Env (SomeUniVal f) -> Expr f a -> Expr f a
normExpr _   expr@EVal{} = expr
normExpr env expr@(EVar u var) =
    case lookupVar var env of
        Nothing -> expr
        Just (SomeUniVal (UniVal u' val)) ->
            withGeqUni u u' (EVal (UniVal u val)) $ error "type mismatch"
normExpr env (EIf e e1 e2) =
    case normExpr env e of
        EVal (UniVal Bool b) -> if b then e1N else e2N
        eN                   -> EIf eN e1N e2N
    where
        e1N = normExpr env e1
        e2N = normExpr env e2
normExpr env (EAppUnOp op e) =
    case normExpr env e of
        EVal (UniVal _ x) -> EVal $ evalUnOp op x
        eN                -> EAppUnOp op eN
normExpr env (EAppBinOp op e1 e2) =
    case (normExpr env e1, normExpr env e2) of
        (EVal (UniVal _ x1), EVal (UniVal _ x2)) -> EVal $ evalBinOp op x1 x2
        (e1N, e2N)                               -> EAppBinOp op e1N e2N
