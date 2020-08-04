{-# LANGUAGE QuasiQuotes  #-}

module TinyLang.Field.Evaluator
    ( EvalError (..)
    , MonadEvalError
    , EvalT
    , Eval
    , execEvalT
    , execEval
    , evalEvalT
    , evalEval
    , lookupVarM
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
    , ProgramWithEnv (..)
    , evalUnOp
    , evalBinOp
    , evalExprUni
    , evalStatements
    , evalStatement
    , evalProgram
    , evalProgWithEnv
    , evalExpr
    , denoteUniConst
    , denoteSomeUniConst
    , denoteExpr
    , normExpr
    , normStatement
    , normStatements
    , normProgram
    ) where

import           Prelude                          hiding (div)

import           TinyLang.Field.Typed.Core
import           TinyLang.Prelude
import           TinyLang.Field.Printer           (Pretty(..))

import qualified Data.String.Interpolate.IsString as QQ
import qualified Data.Vector                      as Vector
import           Data.Kind (Type)

data TypeMismatch f = forall a b. TypeMismatch (UniVar f a) (UniConst f b)

instance TextField f => Show (TypeMismatch f) where
    show (TypeMismatch (UniVar uniExp var) uniConstAct) =
        [QQ.i|Variable #{var} has type #{uniExp}, but was instantiated with #{uniConstAct}|]

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

-- | Our evaluator transformer stack
newtype EvalT f (m :: Type -> Type) a =
    EvalT { unEvalT :: ReaderT (Env (SomeUniConst f)) (StateT [SomeUniConst f] (ExceptT (EvalError f) m)) a }
    deriving newtype ( Monad
                     , Functor
                     , Applicative
                     , MonadError (EvalError f)
                     , MonadState [SomeUniConst f]
                     , MonadReader (Env (SomeUniConst f))
                     )

type Eval f a = EvalT f Identity a

-- NOTE: that the result of evaluation will be reversed to preserve the order
-- | Run @EvalT@ and return its execution trace
execEvalT :: (Monad m) => EvalT f m a -> Env (SomeUniConst f) -> m (Either (EvalError f) [SomeUniConst f])
execEvalT ev r = do
    result <- runExceptT $ flip execStateT mempty $ flip runReaderT r $ unEvalT ev
    pure $ second reverse result
-- | @execEvalT@ specialised for @Identity@
execEval :: Eval f a -> Env (SomeUniConst f) -> Either (EvalError f) [SomeUniConst f]
execEval ev r = runIdentity $ execEvalT ev r

-- | Run @EvalT@ and discard the trace
evalEvalT :: (Monad m) => EvalT f m a -> Env (SomeUniConst f) -> m (Either (EvalError f) a)
evalEvalT ev r = runExceptT $ flip evalStateT mempty $ flip runReaderT r $ unEvalT ev

-- | @evalEvalT@ specialised to @Identity@
evalEval :: Eval f a -> Env (SomeUniConst f) -> Either (EvalError f) a
evalEval ev = runIdentity . evalEvalT ev

-- | lookup variable
lookupVarM :: (Monad m) => Var -> EvalT f m (SomeUniConst f)
lookupVarM var = do
    env <- ask
    fromOption (throwError $ VariableNotInScopeEvalError var) $ lookupVar var env

-- | Bind a variable to @SomeUniConst@ in continuation
withVar :: (Monad m) => Var -> SomeUniConst f -> EvalT f m a -> EvalT f m a
withVar var uniConst = local (insertVar var uniConst)

invEval :: (MonadEvalError g m, Field f) => f -> m f
invEval = fromOption (throwError DivideByZeroEvalError) . inv

divEval :: (MonadEvalError g m, Field f) => f -> f -> m f
divEval = fromOption (throwError DivideByZeroEvalError) .* div

atEval :: MonadEvalError f m => Int -> Vector a -> m a
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

normUniConst :: forall f m a. (Monad m, Field f, AsInteger f)
    => UniConst f a -> EvalT f m (UniConst f a)
normUniConst (UniConst Bool   b) = return $ UniConst Bool   b
normUniConst (UniConst Field  f) = return $ UniConst Field  f
normUniConst (UniConst Vector v) = UniConst Vector <$> normUnpacking v where
    normUnpacking =
        asIntegerEval . toField @(AField f) . packPositiveAsc >=>
            fmap Vector.fromList . unpackPositiveAsc

-- | We want to allow order comparisons on elements of the field, but only
-- if they're integers (whatever that means), and only if they're positive.
-- If we get a non-integer or a negative integer we throw an error.
compareIntegerValues :: (MonadEvalError f m, AsInteger f)
    => (Integer -> Integer -> Bool) -> AField f -> AField f -> m Bool
compareIntegerValues op a b = do
    m <- asPositiveIntegerEval a
    n <- asPositiveIntegerEval b
    return $ m `op` n

evalUnOp :: (MonadEvalError f m, Eq f, Field f, AsInteger f)
    => UnOp f a b -> a -> m (UniConst f b)
evalUnOp Not  = return . UniConst Bool . not
evalUnOp Neq0 = return . UniConst Bool . (/= zer)
evalUnOp Neg  = return . UniConst Field . neg
evalUnOp Inv  = fmap (UniConst Field) . invEval
evalUnOp Unp  = asIntegerEval >=> fmap (UniConst Vector . Vector.fromList) . unpackPositiveAsc

evalBinOp :: (MonadEvalError f m, Eq f, Field f, AsInteger f)
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

-- | Evaluate a program
evalProgram :: (Monad m, Eq f, Field f, AsInteger f)
    => Program f -> EvalT f m ()
evalProgram = flip evalStatements (pure ()) . _programStatements

-- TODO:  Verify whether it is acutally right fold
evalStatements :: (Monad m, Eq f, Field f, AsInteger f)
    => Statements f -> EvalT f m () -> EvalT f m ()
evalStatements (Statements stmts) kont = foldr evalStatement kont stmts

-- | Evaluate a single statement that continue as @kont@
evalStatement :: (Monad m, Eq f, Field f, AsInteger f)
    => Statement f -> EvalT f m () -> EvalT f m ()
evalStatement (ELet (UniVar _ var) def) kont = do
    defR <- evalExprUni def
    let someDefR = Some defR
    modify (someDefR:)
    withVar var someDefR kont
evalStatement (EAssert expr) kont = do
    exprR <- evalExpr expr
    unless exprR $ throwError $ AssertionFailedEvalError expr
    kont

-- Note that we could use dependent maps, but we don't.
-- | A recursive evaluator for expressions. Perhaps simplistic, but it works.
evalExprUni :: (Monad m, Eq f, Field f, AsInteger f)
    => Expr f a -> EvalT f m (UniConst f a)
evalExprUni (EConst uniConst) = normUniConst uniConst
evalExprUni (EVar uniVar@(UniVar uni var)) = do
    Some uniConst@(UniConst uni' _) <- lookupVarM var
    let err = throwError . TypeMismatchEvalError $ TypeMismatch uniVar uniConst
    withGeqUni uni uni' err $ evalExprUni $ EConst uniConst
evalExprUni (EIf e e1 e2) = do
    eR <- evalExpr e
    if eR
        then evalExprUni e1
        else evalExprUni e2
evalExprUni (EAppUnOp op e) = evalExpr e >>= evalUnOp op
evalExprUni (EAppBinOp op e1 e2) = do
    e1R <- evalExpr e1
    e2R <- evalExpr e2
    evalBinOp op e1R e2R

-- | A recursive evaluator for expressions.
evalExpr :: (Monad m, Eq f, Field f, AsInteger f)
    => Expr f a -> EvalT f m a
evalExpr = fmap _uniConstVal . evalExprUni

instance (TextField f) => (Show (Pretty (ProgramWithEnv f))) where
    show (Pretty (ProgramWithEnv prog env)) =
        unlines [ "program:"
                , show . Pretty $ prog
                , "env:"
                , show . Pretty $ env
                ]

data ProgramWithEnv f
    = ProgramWithEnv (Program f) (Env (SomeUniConst f))
      deriving Show via Pretty (ProgramWithEnv f)

-- | Evaluate a program in a given environment
evalProgWithEnv :: (Eq f, Field f, AsInteger f)
    => ProgramWithEnv f -> Either (EvalError f) [SomeUniConst f]
evalProgWithEnv (ProgramWithEnv prog env) =
    execEval (evalProgram prog) env

denoteUniConst :: Field f => UniConst f a -> f
denoteUniConst (UniConst Bool   b) = toField b
denoteUniConst (UniConst Field  i) = unAField i
denoteUniConst (UniConst Vector v) = unAField . fromInteger . packPositiveAsc $ toList v

denoteSomeUniConst :: Field f => SomeUniConst f -> f
denoteSomeUniConst = forget denoteUniConst

denoteExpr :: (Monad m, Eq f, Field f, AsInteger f)
    => Expr f a -> EvalT f m f
denoteExpr = fmap denoteUniConst . evalExprUni

-- | A recursive normalizer for programs.
normProgram :: (Monad m, Eq f, Field f, AsInteger f)
    => Program f -> EvalT f m (Program f)
normProgram (Program exts stmts) = normStatements stmts (pure . Program exts)

-- | A recursive normalizer for statements
normStatements :: (Monad m, Eq f, Field f, AsInteger f)
    => Statements f -> (Statements f -> EvalT f m r) -> EvalT f m r
normStatements (Statements stmts) kont0 = go stmts (kont0 . Statements) where
    go []       kont = kont []
    go (s:rest) kont = normStatement s $ \case
        Nothing -> go rest kont
        Just sN -> go rest $ \restN ->
            kont $ sN : restN

-- | Normalise single statement
normStatement :: (Monad m, Eq f, Field f, AsInteger f)
    => Statement f -> (Maybe (Statement f) -> EvalT f m r) -> EvalT f m r
normStatement (ELet uniVar@(UniVar _ var) def) kont = do
    defN <- normExpr def
    case defN of
        EConst uniConst ->
            withVar var (Some uniConst) $ kont Nothing
        _ ->
            kont . Just $ ELet uniVar defN
normStatement (EAssert expr) kont = do
    exprN <- normExpr expr
    case exprN of
      EConst (UniConst _ False) -> throwError $ AssertionFailedEvalError expr
      EConst (UniConst _ True) -> kont Nothing
      _ -> kont . Just $ EAssert exprN

normExpr :: (Monad m, Eq f, Field f, AsInteger f)
    => Expr f a -> EvalT f m (Expr f a)
normExpr (EConst uniConst) = EConst <$> normUniConst uniConst
normExpr expr@(EVar uniVar@(UniVar uni var)) = do
    env <- ask
    case lookupVar var env of
        Nothing -> return expr
        Just (Some uniConst@(UniConst uni' _)) -> do
            let err = throwError . TypeMismatchEvalError $ TypeMismatch uniVar uniConst
            withGeqUni uni uni' err $ normExpr $ EConst uniConst
normExpr (EIf e e1 e2) = do
    eN <- normExpr e
    let nE1 = normExpr e1
        nE2 = normExpr e2
    case eN of
        EConst (UniConst Bool b) -> if b then nE1 else nE2
        _ -> EIf eN <$> nE1 <*> nE2
normExpr (EAppUnOp op e) = do
    eN <- normExpr e
    case eN of
        EConst (UniConst _ x) -> EConst <$> evalUnOp op x
        _ -> return $ EAppUnOp op eN
normExpr (EAppBinOp op e1 e2) = do
    e1N <- normExpr e1
    e2N <- normExpr e2
    case (e1N, e2N) of
        (EConst (UniConst _ x1), EConst (UniConst _ x2)) ->
            EConst <$> evalBinOp op x1 x2
        _ ->
            return $ EAppBinOp op e1N e2N

