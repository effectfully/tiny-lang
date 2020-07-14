{-# LANGUAGE QuasiQuotes #-}
{-| TypeChecker

We provide a simple bidirectional typechecker

Potential resources:

* https://www.cs.cmu.edu/~fp/courses/15312-f04/handouts/15-bidirectional.pdf

* http://www.cse.chalmers.se/~ulfn/papers/afp08/tutorial.pdf

* http://www.davidchristiansen.dk/tutorials/bidirectional.pdf

-}

module TinyLang.Field.Typed.TypeChecker
    ( TypeCheckError
    , MonadTypeError
    , MonadTypeChecker
    , TypeCheckerT(..)
    , TypeChecker
    , runTypeChecker
    , typeProgram
    )
    where

import           TinyLang.Prelude           hiding (TypeError)

import           Data.Field
import           TinyLang.Field.Existential
import qualified TinyLang.Field.Raw.Core    as R
import qualified TinyLang.Field.Typed.Core  as T
import           TinyLang.Field.UniConst
import           TinyLang.Var

import           Control.Monad.Cont
-- import qualified Data.Set                   as Set
import           Data.Kind
import qualified Data.Map.Strict            as Map
import qualified Data.String.Interpolate    as QQ

{-| == Utility Type Aliases or Constraints
-}

type TypeCheckError        = String
type MonadTypeError   m    = MonadError TypeCheckError m
type MonadTypeChecker m f  = ( MonadSupply m
                             , MonadTypeError m
                             , MonadReader TyEnv m
                             )

{-| == Type Environments
-}

-- TODO: Can be parameterised later
type TyEnv = Map R.Var T.Var


{-| @TypeChecker@ Transformer
-}
newtype TypeCheckerT (m :: Type -> Type) a =
    TypeChecker { runTypeCheckerT :: (ExceptT TypeCheckError (ReaderT TyEnv (SupplyT m))) a }
    deriving newtype ( Monad
                     , Functor
                     , Applicative
                     , MonadError TypeCheckError
                     , MonadSupply
                     , MonadReader TyEnv
                     )

{-| A simple type checker
-}
type TypeChecker = TypeCheckerT Identity

{-| Run a type checker function. Note that if there are several variables with the same textual name
then the resulting scope will only contain the last one.
-}
runTypeChecker :: (MonadError TypeCheckError m, MonadSupply m) => TypeChecker a -> m a
runTypeChecker typeChecker =
    liftSupply ( flip runReaderT mempty
               . runExceptT
               . runTypeCheckerT
               $ typeChecker
               )
        >>= liftEither

{-|
-}
typeProgram ::
    (MonadError TypeCheckError m, MonadSupply m, TextField f)
    => R.Program R.Var f -> m (T.Program f)
typeProgram = runTypeChecker . checkProgram

{-| Add a variable to type environment
-}
-- NOTE:  At the moment this mimics the old scope
-- TODO:  Discuss wether we should add an env on top of @withVar@ or not.
withVar :: (Monad m) => R.Var -> forall r. (T.Var -> TypeCheckerT m r) -> TypeCheckerT m r
withVar var kont = do
    tyEnv <- ask
    case Map.lookup var tyEnv of
        Just tVar -> kont tVar
        Nothing -> do
            tVar <- freshVar . R.unVar $ var
            local (Map.insert var tVar) $ kont tVar

{-| Type inference for variables
-}
inferUniVar :: (Monad m) => R.Var -> TypeCheckerT m (T.SomeUniVar f)
inferUniVar var = do
    tyEnv <- ask
    case Map.lookup var tyEnv of
        Just tVar -> pure . mkSomeUniVar $ tVar
        Nothing -> throwError $ unboundVariable var tyEnv
       
{-| Type inference for expressions
-}
inferExpr ::
    forall m f. (Monad m, TextField f)
    => R.Expr R.Var f -> TypeCheckerT m (T.SomeUniExpr f)
inferExpr (R.EConst (Some c@(T.UniConst uni _))) =
    pure $ SomeOf uni $ T.EConst c
inferExpr (R.EVar v) = do
    Some uniVar@(T.UniVar uni _) <- inferUniVar v
    pure $ SomeOf uni $ T.EVar uniVar
inferExpr (R.EAppBinOp rBinOp l m) =
    withTypedBinOp rBinOp $ \tBinOp ->
        SomeOf knownUni <$> (T.EAppBinOp tBinOp <$> checkExpr l <*> checkExpr m)
inferExpr (R.EAppUnOp rUnOp l) =
    withTypedUnOp rUnOp $ \tUnOp ->
        SomeOf knownUni <$> (T.EAppUnOp tUnOp <$> checkExpr l)
inferExpr (R.EIf l m n) = do
    tL <- checkExpr l
    SomeOf uni tM <- inferExpr m
    tN <- T.withKnownUni uni $ checkExpr n
    pure $ SomeOf uni $ T.EIf tL tM tN
inferExpr (R.ETypeAnn (Some uni) m) =
    T.withKnownUni uni $ SomeOf uni <$> checkExpr m

{-| Mapping from Raw UnOp to Typed UnOp
-}
withTypedBinOp ::
    forall f r.
    R.BinOp -> (forall a b c. ( KnownUni f a, KnownUni f b, KnownUni f c) => T.BinOp f a b c -> r) -> r
withTypedBinOp R.Or  k = k T.Or
withTypedBinOp R.And k = k T.And
withTypedBinOp R.Xor k = k T.Xor
withTypedBinOp R.FEq k = k T.FEq
withTypedBinOp R.FLt k = k T.FLt
withTypedBinOp R.FLe k = k T.FLe
withTypedBinOp R.FGe k = k T.FGe
withTypedBinOp R.FGt k = k T.FGt
withTypedBinOp R.Add k = k T.Add
withTypedBinOp R.Sub k = k T.Sub
withTypedBinOp R.Mul k = k T.Mul
withTypedBinOp R.Div k = k T.Div
withTypedBinOp R.BAt k = k T.BAt

{-| Mapping from Raw UnOp to Typed UnOp
-}
withTypedUnOp ::
    forall f r.
    R.UnOp -> (forall a b. (KnownUni f a, KnownUni f b) => T.UnOp f a b -> r) -> r
withTypedUnOp R.Not  k = k T.Not
withTypedUnOp R.Neq0 k = k T.Neq0
withTypedUnOp R.Neg  k = k T.Neg
withTypedUnOp R.Inv  k = k T.Inv
withTypedUnOp R.Unp  k = k T.Unp

{-| Type checking for expressions
-}
checkExpr ::
    forall m f a. (Monad m, TextField f, KnownUni f a)
    => R.Expr R.Var f -> TypeCheckerT m (T.Expr f a)
checkExpr (R.EIf l m n) = T.EIf <$> checkExpr l <*> checkExpr m <*> checkExpr n
checkExpr m = do
    SomeOf mUni tM <- inferExpr m
    let uni = knownUni @f @a
    let uniMismatch = typeMismatch tM uni mUni
    withGeqUniM uni mUni uniMismatch tM

checkProgram ::
    forall m f. (Monad m, TextField f)
    => R.Program R.Var f -> TypeCheckerT m (T.Program f)
checkProgram (R.Program exts stmts) =
    runContT (traverse (ContT . withVar) exts) $ \tVars -> checkStatements stmts (pure . T.Program tVars)

checkStatements ::
    forall m f. (Monad m , TextField f)
    => R.Statements R.Var f -> forall r. (T.Statements f -> TypeCheckerT m r) -> TypeCheckerT m r
checkStatements (R.Statements stmts) kont =
    runContT (foldMapA (ContT . checkStatement) stmts) $ kont . T.Statements

{-| Type checking judgement for statements of form
-}
checkStatement ::
    forall m f. (Monad m , TextField f)
    => R.Statement R.Var f -> forall r. ([T.Statement f] -> TypeCheckerT m r) -> TypeCheckerT m r
checkStatement (R.ELet var m) kont =
    case uniOfVar . R.unVar $ var of
        Some uni -> do
                tM <- T.withKnownUni uni $ checkExpr m
                withVar var $ \ tVar -> kont [T.ELet (UniVar uni tVar) tM]
checkStatement (R.EAssert m) kont = do
    tM <- checkExpr m
    kont [T.EAssert tM]
checkStatement (R.EFor var start end stmts) kont = do
    runContT (foldMapA (ContT . iter) [start .. end]) kont where
        iter i ikont = withVar var $ \ tVar ->
            checkStatements stmts $ \ (T.Statements tStmts) -> do
                let uVar = T.UniVar Field tVar
                ikont $ T.ELet uVar (T.EConst . fromIntegral $ i) : tStmts

{-| Error message for a failed type equality
-}
typeMismatch ::
    forall a b c. (Show a, Show b, Show c)
    => a -> b -> c -> TypeCheckError
typeMismatch expr expected found =
    [QQ.i|error: Universe mismatch for expression:
           #{show expr}
         Expected: #{show expected}
         Found:    #{show found}|]

unboundVariable :: R.Var -> TyEnv -> TypeCheckError
unboundVariable var tyEnv =
    [QQ.i|error: Unbound variable:
           #{R.unVar var}
         Environment: #{show tyEnv}|]
