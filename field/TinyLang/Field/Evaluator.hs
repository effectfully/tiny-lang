{-# LANGUAGE QuasiQuotes #-}

module TinyLang.Field.Evaluator
    ( EvalError (..)
    , MonadEvalError
    , lookupVarEval
    , invEval
    , divEval
    , atEval
    , asIntegerEval
    , asPositiveEval
    , asPositiveIntegerEval
    , unpackPositiveAsc
    , unpackPositiveDesc
    , packPositiveAsc
    , packPositiveDesc
    , ExprWithEnv (..)
    , evalUnOp
    , evalBinOp
    , evalExprUni
    , evalExpr
    , evalExprWithEnv
    , denoteUniConst
    , denoteSomeUniConst
    , denoteExpr
    , normExpr
    , instStatement
    , instExpr
    ) where

import           Prelude              hiding (div)

import           TinyLang.Environment
import           TinyLang.Field.Typed.Core
import           TinyLang.Prelude

import qualified Data.Vector          as Vector
import qualified Data.String.Interpolate.IsString as QQ

data TypeMismatch f = forall a b. TypeMismatch (UniVar f a) (UniConst f b)

data EvalError f
    = VariableNotInScopeEvalError Var
    | IndexTooLargeEvalError Int
    | UnpackNegativeEvalError Integer
    | NonIntegerAsIntegerEvalError (AField f)
    | DivideByZeroEvalError
    | NegativeAsPositiveEvalError Integer
    | AssertionFailedEvalError (Expr f Bool)
    | TypeMismatchEvalError (TypeMismatch f)
    -- TODO: proper error messages.
    deriving (Show)

type MonadEvalError f m = MonadError (EvalError f) m

instance TextField f => Show (TypeMismatch f) where
    show (TypeMismatch (UniVar uniExp var) uniConstAct) =
        [QQ.i|Variable #{var} has type #{uniExp}, but was instantiated with #{uniConstAct}|]

lookupVarEval :: MonadEvalError g m => Var -> Env f -> m f
lookupVarEval var = fromOption (throwError $ VariableNotInScopeEvalError var) . lookupVar var

invEval :: (MonadEvalError g m, Field f) => f -> m f
invEval = fromOption (throwError DivideByZeroEvalError) . inv

divEval :: (MonadEvalError g m, Field f) => f -> f -> m f
divEval = fromOption (throwError DivideByZeroEvalError) .* div

atEval :: MonadEvalError g m => Int -> Vector a -> m a
atEval i xs = fromOption (throwError $ IndexTooLargeEvalError i) $ xs Vector.!? i

asIntegerEval :: (MonadEvalError f m, AsInteger f) => AField f -> m Integer
asIntegerEval x = fromOption (throwError $ NonIntegerAsIntegerEvalError x) $ asInteger x

asPositiveEval :: MonadEvalError f m => Integer -> m Integer
asPositiveEval i
    | i >= 0    = pure i
    | otherwise = throwError $ NegativeAsPositiveEvalError i

asPositiveIntegerEval :: (MonadEvalError f m, AsInteger f) => AField f -> m Integer
asPositiveIntegerEval = asIntegerEval >=> asPositiveEval

unpackPositiveAsc :: MonadEvalError f m => Integer -> m [Bool]
unpackPositiveAsc n0 | n0 < 0 = throwError $ UnpackNegativeEvalError n0
unpackPositiveAsc n0          = map (== 1) <$> go n0 where
    go n | n < 2 = return [n]
    go n         = (r :) <$> go q where
        (q, r) = n `quotRem` 2

unpackPositiveDesc :: MonadEvalError f m => Integer -> m [Bool]
unpackPositiveDesc = fmap reverse . unpackPositiveAsc

-- Note that this leaks due to '(,)' being lazy.
packPositiveAsc :: Foldable f => f Bool -> Integer
packPositiveAsc = fst . foldl' (\(a, p) b -> (a + if b then p else 0, p * 2)) (0, 1)

packPositiveDesc :: [Bool] -> Integer
packPositiveDesc = packPositiveAsc . reverse

normUniConst
    :: forall f m a. (MonadEvalError f m, Field f, AsInteger f)
    => UniConst f a -> m (UniConst f a)
normUniConst (UniConst Bool   b) = return $ UniConst Bool   b
normUniConst (UniConst Field  f) = return $ UniConst Field  f
normUniConst (UniConst Vector v) = UniConst Vector <$> normUnpacking v where
    normUnpacking =
        asIntegerEval . toField @(AField f) . packPositiveAsc >=>
            fmap (Vector.fromList) . unpackPositiveAsc

-- | We want to allow order comparisons on elements of the field, but only
-- if they're integers (whatever that means), and only if they're positive.
-- If we get a non-integer or a negative integer we throw an error.
compareIntegerValues
    :: (MonadEvalError f m, AsInteger f)
    => (Integer -> Integer -> Bool) -> AField f -> AField f -> m Bool
compareIntegerValues op a b = do
    m <- asPositiveIntegerEval a
    n <- asPositiveIntegerEval b
    return $ m `op` n

evalUnOp
    :: (MonadEvalError f m, Eq f, Field f, AsInteger f)
    => UnOp f a b -> a -> m (UniConst f b)
evalUnOp Not  = return . UniConst Bool . not
evalUnOp Neq0 = return . UniConst Bool . (/= zer)
evalUnOp Neg  = return . UniConst Field . neg
evalUnOp Inv  = fmap (UniConst Field) . invEval
evalUnOp Unp  = asIntegerEval >=> fmap (UniConst Vector . Vector.fromList) . unpackPositiveAsc

evalBinOp
    :: (MonadEvalError f m, Eq f, Field f, AsInteger f)
    => BinOp f a b c -> a -> b -> m (UniConst f c)
evalBinOp Or  x y = return . UniConst Bool $ x || y
evalBinOp And x y = return . UniConst Bool $ x && y
evalBinOp Xor x y = return . UniConst Bool $ x /= y
evalBinOp FEq x y = return . UniConst Bool $ x == y
evalBinOp FLt x y = UniConst Bool <$> compareIntegerValues (<)  x y
evalBinOp FLe x y = UniConst Bool <$> compareIntegerValues (<=) x y
evalBinOp FGt x y = UniConst Bool <$> compareIntegerValues (>)  x y
evalBinOp FGe x y = UniConst Bool <$> compareIntegerValues (>=) x y
evalBinOp Add x y = return . UniConst Field $ x `add` y
evalBinOp Sub x y = return . UniConst Field $ x `sub` y
evalBinOp Mul x y = return . UniConst Field $ x `mul` y
evalBinOp Div x y = UniConst Field <$> divEval x y
evalBinOp BAt x y = asIntegerEval x >>= fmap (UniConst Bool) . flip atEval y . fromIntegral

evalStatementUni
    :: (MonadEvalError f m, Eq f, Field f, AsInteger f)
    => Env (SomeUniConst f) -> Statement f -> Expr f a -> m (UniConst f a)
evalStatementUni env (ELet (UniVar _ var) def) rest = do
    defR <- evalExprUni env def
    evalExprUni (insertVar var (Some defR) env) rest
evalStatementUni env (EAssert expr) rest = do
    exprR <- evalExpr env expr
    if exprR
        then evalExprUni env rest
        else throwError $ AssertionFailedEvalError expr

-- Note that we could use dependent maps, but we don't.
-- | A recursive evaluator for expressions. Perhaps simplistic, but it works.
evalExprUni
    :: (MonadEvalError f m, Eq f, Field f, AsInteger f)
    => Env (SomeUniConst f) -> Expr f a -> m (UniConst f a)
evalExprUni _   (EConst uniConst) = normUniConst uniConst
evalExprUni env (EVar uniVar@(UniVar uni var)) = do
    Some uniConst@(UniConst uni' _) <- lookupVarEval var env
    let err = throwError . TypeMismatchEvalError $ TypeMismatch uniVar uniConst
    withGeqUni uni uni' err $ evalExprUni env $ EConst uniConst
evalExprUni env (EIf e e1 e2) = do
    eR <- evalExpr env e
    if eR
        then evalExprUni env e1
        else evalExprUni env e2
evalExprUni env (EAppUnOp op e) = evalExpr env e >>= evalUnOp op
evalExprUni env (EAppBinOp op e1 e2) = do
    e1R <- evalExpr env e1
    e2R <- evalExpr env e2
    evalBinOp op e1R e2R
evalExprUni env (EStatement stat expr) = evalStatementUni env stat expr

-- | A recursive evaluator for expressions.
evalExpr
    :: (MonadEvalError f m, Eq f, Field f, AsInteger f)
    => Env (SomeUniConst f) -> Expr f a -> m a
evalExpr env = fmap _uniConstVal . evalExprUni env

-- | A type of expressions together with environments
data ExprWithEnv f
    = ExprWithEnv (SomeUniExpr f) (Env (SomeUniConst f))
      deriving (Show)

-- | Evaluate an expression in a given environment
evalExprWithEnv
    :: (MonadEvalError f m, Eq f, Field f, AsInteger f)
    => ExprWithEnv f -> m (SomeUniConst f)
evalExprWithEnv (ExprWithEnv (SomeOf uni expr) env) =
    Some . UniConst uni <$> evalExpr env expr

denoteUniConst :: Field f => UniConst f a -> f
denoteUniConst (UniConst Bool   b) = toField b
denoteUniConst (UniConst Field  i) = unAField i
denoteUniConst (UniConst Vector v) = unAField . fromInteger . packPositiveAsc $ toList v

denoteSomeUniConst :: Field f => SomeUniConst f -> f
denoteSomeUniConst = forget denoteUniConst

denoteExpr
    :: (MonadEvalError f m, Eq f, Field f, AsInteger f)
    => Env (SomeUniConst f) -> Expr f a -> m f
denoteExpr env = fmap denoteUniConst . evalExprUni env

normStatement
    :: (MonadEvalError f m, Eq f, Field f, AsInteger f)
    => Env (SomeUniConst f) -> Statement f -> Expr f a -> m (Expr f a)
normStatement env (ELet (UniVar uni var) def) rest = do
    defN <- normExpr env def
    case defN of
        EConst uniConst -> normExpr (insertVar var (Some uniConst) env) rest
        _             -> EStatement (ELet (UniVar uni var) defN) <$> normExpr env rest
normStatement env (EAssert expr) rest = do
    exprN <- normExpr env expr
    EStatement (EAssert exprN) <$> normExpr env rest

-- | A recursive normalizer for expressions.
normExpr
    :: (MonadEvalError f m, Eq f, Field f, AsInteger f)
    => Env (SomeUniConst f) -> Expr f a -> m (Expr f a)
normExpr _   (EConst uniConst) = EConst <$> normUniConst uniConst
normExpr env expr@(EVar uniVar@(UniVar uni var)) =
    case lookupVar var env of
        Nothing -> return expr
        Just (Some uniConst@(UniConst uni' _)) -> do
            let err = throwError . TypeMismatchEvalError $ TypeMismatch uniVar uniConst
            withGeqUni uni uni' err $ normExpr env $ EConst uniConst
normExpr env (EIf e e1 e2) = do
    eN <- normExpr env e
    let nE1 = normExpr env e1
        nE2 = normExpr env e2
    case eN of
        EConst (UniConst Bool b) -> if b then nE1 else nE2
        _                      -> EIf eN <$> nE1 <*> nE2
normExpr env (EAppUnOp op e) = do
    eN <- normExpr env e
    case eN of
        EConst (UniConst _ x) -> EConst <$> evalUnOp op x
        _                   -> return $ EAppUnOp op eN
normExpr env (EAppBinOp op e1 e2) = do
    e1N <- normExpr env e1
    e2N <- normExpr env e2
    case (e1N, e2N) of
        (EConst (UniConst _ x1), EConst (UniConst _ x2)) ->
            EConst <$> evalBinOp op x1 x2
        _                                            ->
            return $ EAppBinOp op e1N e2N
normExpr env (EStatement stat expr) = normStatement env stat expr

-- | Instantiate some of the variables of a statement with values.
instStatement
    :: MonadEvalError f m => Env (SomeUniConst f) -> Statement f -> m (Statement f)
instStatement env (ELet uniVar def) = ELet uniVar <$> instExpr env def
instStatement env (EAssert expr)    = EAssert <$> instExpr env expr

-- | Instantiate some of the variables of an expression with values.
instExpr :: MonadEvalError f m => Env (SomeUniConst f) -> Expr f a -> m (Expr f a)
instExpr _   expr@(EConst _) = return expr
instExpr env expr@(EVar uniVar@(UniVar uni var)) =
    case lookupVar var env of
        Nothing -> return expr
        Just (Some uniConst@(UniConst uni' _)) -> do
            let err = throwError . TypeMismatchEvalError $ TypeMismatch uniVar uniConst
            withGeqUni uni uni' err $ return $ EConst uniConst
instExpr env (EIf e e1 e2) = EIf <$> instExpr env e <*> instExpr env e1 <*> instExpr env e2
instExpr env (EAppUnOp op e) = EAppUnOp op <$> instExpr env e
instExpr env (EAppBinOp op e1 e2) = EAppBinOp op <$> instExpr env e1 <*> instExpr env e2
instExpr env (EStatement stat expr) = EStatement <$> instStatement env stat <*> instExpr env expr
