module TinyLang.Field.Evaluator
    ( ExprWithEnv (..)
    , evalUnOp
    , evalBinOp
    , evalExprUni
    , evalExpr
    , evalExprWithEnv
    , denoteUniVal
    , denoteSomeUniVal
    , denoteExpr
    , normExpr
    , instStatement
    , instExpr
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

checkEConstr :: (Eq f, Field f, AsInteger f) => Env (SomeUniVal f) -> EConstr f -> Bool
checkEConstr env (EConstrFEq lhs rhs) = evalExpr env lhs == evalExpr env rhs

evalStatementUni
    :: (Eq f, Field f, AsInteger f)
    => Env (SomeUniVal f) -> Statement f -> Expr f a -> UniVal f a
evalStatementUni env (ELet (UniVar _ var) def) expr =
    evalExprUni (insertVar var (Some $ evalExprUni env def) env) expr
evalStatementUni env (EConstr econstr) expr =
    if checkEConstr env econstr
        then evalExprUni env expr
        else throw Denormal

-- Note that we could use dependent maps, but we don't.
-- | A recursive evaluator for expressions. Perhaps simplistic, but it works.
evalExprUni :: (Eq f, Field f, AsInteger f) => Env (SomeUniVal f) -> Expr f a -> UniVal f a
evalExprUni _   (EVal uniVal) = uniVal
evalExprUni env (EVar (UniVar uni var)) = case unsafeLookupVar var env of
    Some uniVal@(UniVal uni' _) -> withGeqUni uni uni' (error "type mismatch") uniVal
evalExprUni env (EIf e e1 e2) = if evalExpr env e then evalExprUni env e1 else evalExprUni env e2
evalExprUni env (EAppUnOp op e) = evalUnOp op (evalExpr env e)
evalExprUni env (EAppBinOp op e1 e2) =
    evalBinOp op (evalExpr env e1) (evalExpr env e2)
evalExprUni env (EStatement stat expr) = evalStatementUni env stat expr

-- | A recursive evaluator for expressions.
evalExpr :: (Eq f, Field f, AsInteger f) => Env (SomeUniVal f) -> Expr f a -> a
evalExpr env = _uniValVal . evalExprUni env

-- | A type of expressions together with environments
data ExprWithEnv f
    = ExprWithEnv (SomeUniExpr f) (Env (SomeUniVal f))
      deriving (Show)

-- | Evaluate an expression in a given environment
evalExprWithEnv :: (Eq f, Field f, AsInteger f) => ExprWithEnv f -> SomeUniVal f
evalExprWithEnv (ExprWithEnv (SomeUniExpr uni expr) env) =
    Some . UniVal uni $ evalExpr env expr

denoteUniVal :: Field f => UniVal f a -> f
denoteUniVal (UniVal Bool  b) = boolToField b
denoteUniVal (UniVal Field i) = unAField i

denoteSomeUniVal :: Field f => SomeUniVal f -> f
denoteSomeUniVal (Some uniVal) = denoteUniVal uniVal

denoteExpr :: (Eq f, Field f, AsInteger f) => Env (SomeUniVal f) -> Expr f a -> f
denoteExpr env = denoteUniVal . evalExprUni env

normEConstr :: (Eq f, Field f, AsInteger f) => Env (SomeUniVal f) -> EConstr f -> EConstr f
normEConstr env (EConstrFEq lhs rhs) = EConstrFEq (normExpr env lhs) (normExpr env rhs)

normStatement
    :: (Eq f, Field f, AsInteger f)
    => Env (SomeUniVal f) -> Statement f -> Expr f a -> Expr f a
normStatement env (ELet (UniVar uni var) def) expr =
    case normExpr env def of
        EVal uniVal -> normExpr (insertVar var (Some uniVal) env) expr
        def'        -> EStatement (ELet (UniVar uni var) def') $ normExpr env expr
normStatement env (EConstr econstr) expr =
    EStatement (EConstr $ normEConstr env econstr) $ normExpr env expr

-- | A recursive normalizer for expressions.
normExpr :: (Eq f, Field f, AsInteger f) => Env (SomeUniVal f) -> Expr f a -> Expr f a
normExpr _   expr@EVal{} = expr
normExpr env expr@(EVar (UniVar uni var)) =
    case lookupVar var env of
        Nothing -> expr
        Just (Some (UniVal uni' val)) ->
            withGeqUni uni uni' (error "type mismatch") $ EVal (UniVal uni val)
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
normExpr env (EStatement stat expr) = normStatement env stat expr

-- | Instantiate some of the variables of a statement with values.
instStatement :: Env (SomeUniVal f) -> Statement f -> Statement f
instStatement env (ELet uniVar def) = ELet uniVar $ instExpr env def
instStatement env (EConstr econstr) = EConstr $ case econstr of
    EConstrFEq lhs rhs -> EConstrFEq (instExpr env lhs) (instExpr env rhs)

-- | Instantiate some of the variables of an expression with values.
instExpr :: Env (SomeUniVal f) -> Expr f a -> Expr f a
instExpr _   expr@(EVal _) = expr
instExpr env expr@(EVar (UniVar uni var)) = case lookupVar var env of
    Nothing -> expr
    Just (Some uniVal@(UniVal uni' _)) ->
        withGeqUni uni uni' (error "type mismatch") $ EVal uniVal
instExpr env (EIf e e1 e2) = EIf (instExpr env e) (instExpr env e1) (instExpr env e2)
instExpr env (EAppUnOp op e) = EAppUnOp op (instExpr env e)
instExpr env (EAppBinOp op e1 e2) = EAppBinOp op (instExpr env e1) (instExpr env e2)
instExpr env (EStatement stat expr) = EStatement (instStatement env stat) (instExpr env expr)
