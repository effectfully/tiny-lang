module TinyLang.Field.Evaluator
    ( SomeUniVal (..)
    , ExprWithEnv (..)
    , evalUnOp
    , evalBinOp
    , evalExprUni
    , evalExpr
    , evalExprWithEnv
    , denoteUniVal
    , denoteSomeUniVal
    , denoteExpr
    , normExpr
    ) where

import           Prelude              hiding (div)

import           TinyLang.Environment
import           TinyLang.Field.Core
import           TinyLang.Prelude


-- | We want to allow order comparisons on elements of the field, but only
-- if they're integers (whatever that means), and only if they're positive.
-- If we get a non-integer we throw Denormal, and if we get something negative
-- we throw Underflow. Maybe we want our own exceptions here.
compareIntegerValues :: AsInteger f => (Integer -> Integer -> Bool) -> f -> f -> Bool
compareIntegerValues op a b =
    case (asInteger a, asInteger b) of
      (Just m, Just n) ->
          if m<0 || n<0
          then throw Underflow
          else op m n
      _                -> throw Denormal

evalUnOp :: (Eq f, Field f) => UnOp f a b -> a -> UniVal f b
evalUnOp Not  = UniVal Bool . not
evalUnOp Neq0 = UniVal Bool . (/= zer)
evalUnOp Neg  = UniVal Field . neg
evalUnOp Inv  = UniVal Field . inv

evalBinOp :: (Eq f, Field f, AsInteger f) => BinOp f a b c -> a -> b -> UniVal f c
evalBinOp Or  = UniVal Bool .* (||)
evalBinOp And = UniVal Bool .* (&&)
evalBinOp Xor = UniVal Bool .* (/=)
evalBinOp FEq = UniVal Bool .* (==)
evalBinOp FLt = UniVal Bool .* compareIntegerValues (<)
evalBinOp FLe = UniVal Bool .* compareIntegerValues (<=)
evalBinOp FGe = UniVal Bool .* compareIntegerValues (>=)
evalBinOp FGt = UniVal Bool .* compareIntegerValues (>)
evalBinOp Add = UniVal Field .* add
evalBinOp Sub = UniVal Field .* sub
evalBinOp Mul = UniVal Field .* mul
evalBinOp Div = UniVal Field .* div

-- Note that we could use dependent maps, but we don't.
-- | A recursive evaluator for expressions. Perhaps simplistic, but it works.
evalExprUni :: (Eq f, Field f, AsInteger f) => Env (SomeUniVal f) -> Expr f a -> UniVal f a
evalExprUni _   (EVal uniVal) = uniVal
evalExprUni env (EVar (UniVar uni var)) = case unsafeLookupVar var env of
    SomeUniVal uniVal@(UniVal uni' _) -> withGeqUni uni uni' uniVal $ error "type mismatch"
evalExprUni env (EIf e e1 e2) = if evalExpr env e then evalExprUni env e1 else evalExprUni env e2
evalExprUni env (EAppUnOp op e) = evalUnOp op (evalExpr env e)
evalExprUni env (EAppBinOp op e1 e2) =
    evalBinOp op (evalExpr env e1) (evalExpr env e2)
evalExprUni env (ELet (UniVar _ var) def expr) =
    evalExprUni (insertVar var (SomeUniVal $ evalExprUni env def) env) expr

-- | A recursive evaluator for expressions.
evalExpr :: (Eq f, Field f, AsInteger f) => Env (SomeUniVal f) -> Expr f a -> a
evalExpr env = _uniValVal . evalExprUni env

-- | A type of expressions together with environments
data ExprWithEnv f
    = ExprWithEnv (SomeUniExpr f) (Env (SomeUniVal f))
      deriving (Show)

-- | Evaluate an expression in a given environment
evalExprWithEnv :: (Eq f, Field f, AsInteger f) => ExprWithEnv f -> SomeUniVal f
evalExprWithEnv (ExprWithEnv (SomeUniExpr t e) env) =
          SomeUniVal (UniVal t (TinyLang.Field.Evaluator.evalExpr env e))


denoteUniVal :: Field f => UniVal f a -> f
denoteUniVal (UniVal Bool  b) = boolToField b
denoteUniVal (UniVal Field i) = unAField i

denoteSomeUniVal :: Field f => SomeUniVal f -> f
denoteSomeUniVal (SomeUniVal uniVal) = denoteUniVal uniVal

denoteExpr :: (Eq f, Field f, AsInteger f) => Env (SomeUniVal f) -> Expr f a -> f
denoteExpr env = denoteUniVal . evalExprUni env

-- | A recursive normalizer for expressions.
normExpr :: (Eq f, Field f, AsInteger f) => Env (SomeUniVal f) -> Expr f a -> Expr f a
normExpr _   expr@EVal{} = expr
normExpr env expr@(EVar (UniVar uni var)) =
    case lookupVar var env of
        Nothing -> expr
        Just (SomeUniVal (UniVal uni' val)) ->
            withGeqUni uni uni' (EVal $ UniVal uni val) $ error "type mismatch"
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
normExpr env (ELet (UniVar uni var) def expr) =
    case normExpr env def of
        EVal uniVal -> normExpr (insertVar var (SomeUniVal uniVal) env) expr
        def'        -> ELet (UniVar uni var) def' $ normExpr env expr
