module TinyLang.Field.Evaluator
    ( SomeUniVal (..)
    , evalUnOp
    , evalBinOp
    , evalExpr
    , normExpr
    ) where

import           Prelude               hiding (div)
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

data SomeUniVal f = forall a. SomeUniVal (Uni f a) a

-- Note that we could use dependent maps, but we don't.
-- | A recursive evaluator for expressions. Perhaps simplistic, but it works.
evalExpr :: (Eq f, Field f) => Env (SomeUniVal f) -> Expr f a -> a
evalExpr _   (EVal (UniVal _ x)) = x
evalExpr env (EVar u var) = case unsafeLookupVar var env of
    SomeUniVal u' val -> withGeqUni u u' val $ error "type mismatch"
evalExpr env (EIf e e1 e2) = if evalExpr env e then evalExpr env e1 else evalExpr env e2
evalExpr env (EAppUnOp op e) = _uniValVal $ evalUnOp op (evalExpr env e)
evalExpr env (EAppBinOp op e1 e2) =
    _uniValVal $ evalBinOp op (evalExpr env e1) (evalExpr env e2)

-- | A recursive normalizer for expressions.
normExpr :: (Eq f, Field f) => Env (SomeUniVal f) -> Expr f a -> Expr f a
normExpr _   expr@EVal{} = expr
normExpr env expr@(EVar u var) =
    case lookupVar var env of
        Nothing -> expr
        Just (SomeUniVal u' val) -> withGeqUni u u' (EVal (UniVal u val)) $ error "type mismatch"
normExpr env (EIf e e1 e2) =
    case normExpr env e of
        EVal (UniVal Bool b) -> if b then e1N else e2N
        eN -> EIf eN e1N e2N
    where
        e1N = normExpr env e1
        e2N = normExpr env e2
normExpr env (EAppUnOp op e) =
    case normExpr env e of
        EVal (UniVal _ x) -> EVal $ evalUnOp op x
        eN -> EAppUnOp op eN
normExpr env (EAppBinOp op e1 e2) =
    case (normExpr env e1, normExpr env e2) of
        (EVal (UniVal _ x1), EVal (UniVal _ x2)) -> EVal $ evalBinOp op x1 x2
        (e1N, e2N) -> EAppBinOp op e1N e2N
