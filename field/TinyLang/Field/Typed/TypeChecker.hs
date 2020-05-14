{-# LANGUAGE QuasiQuotes #-}
{-| TypeChecker

We provide a simple bidirectional typechecker

Potential resources:

* https://www.cs.cmu.edu/~fp/courses/15312-f04/handouts/15-bidirectional.pdf

* http://www.cse.chalmers.se/~ulfn/papers/afp08/tutorial.pdf

* http://www.davidchristiansen.dk/tutorials/bidirectional.pdf

-}

module TinyLang.Field.Typed.TypeChecker
    ( Scope
    , MonadScope
    , Scoped (..)
    , TypeCheckError
    , MonadTypeError
    , MonadTypeChecker
    , TypeCheckerT(..)
    , TypeChecker
    , runTypeChecker
    , typeCheck
    , checkType
    , typeProgram
    , inferExpr
    , checkExpr
    , inferUniVar
    , checkUniVar
    , checkStatement
    , checkProgram
    )
    where

import           TinyLang.Prelude           hiding (TypeError)

import           Data.Field
import           TinyLang.Field.Existential
import qualified TinyLang.Field.Raw.Core    as R
import qualified TinyLang.Field.Typed.Core  as T
import           TinyLang.Field.UniConst
import           TinyLang.Var

import           Data.Kind
import qualified Data.Map.Strict            as Map
import qualified Data.String.Interpolate    as QQ

-- | 'Scope' maps names onto 'Var's.
type Scope = Map String Var
type MonadScope = MonadState Scope

data Scoped a = Scoped
    { _scopedScope :: Map String Var
    , _scopedValue :: a
    } deriving (Functor, Foldable, Traversable)

{-| == Utility Type Aliases or Constraints
-}

type TypeCheckError     = String
type MonadTypeError   m = MonadError TypeCheckError m
type MonadTypeChecker m = ( MonadSupply m
                          , MonadScope m
                          , MonadTypeError m
                          )

{-| @TypeChecker@ Transformer
-}
newtype TypeCheckerT e (m :: Type -> Type) a =
    TypeChecker { runTypeCheckerT :: (ExceptT e (StateT Scope (SupplyT m))) a }
    deriving newtype ( Monad
                     , Functor
                     , Applicative
                     , MonadError e
                     , MonadSupply
                     , MonadScope
                     )

{-| A simple type checker
-}
type TypeChecker = TypeCheckerT TypeCheckError Identity

{-| Run a type checker function. Note that if there are several variables with the same textual name
then the resulting scope will only contain the last one.
-}
runTypeChecker :: (MonadError TypeCheckError m, MonadSupply m) => TypeChecker a -> m (Scoped a)
runTypeChecker typeChecker =
    liftSupply (runStateT (runExceptT $ runTypeCheckerT typeChecker) mempty)
        >>= \(errOrRes, scope) -> Scoped scope <$> liftEither errOrRes

{-|
-}
typeCheck
    :: (MonadError TypeCheckError m, MonadSupply m, TextField f)
    => R.Expr R.Var f -> m (Scoped (T.SomeUniExpr f))
typeCheck = runTypeChecker . inferExpr

{-|
-}
typeProgram
    :: (MonadError TypeCheckError m, MonadSupply m, TextField f)
    => R.Program R.Var f -> m (Scoped (T.Program f))
typeProgram = runTypeChecker . checkProgram

{-|
-}
checkType
    :: (MonadError TypeCheckError m, MonadSupply m, TextField f, KnownUni f a)
    => R.Expr R.Var f -> m (Scoped (T.Expr f a))
checkType = runTypeChecker . checkExpr

{-| Look up a variable name. If we've already seen it, return the corresponding Var;
otherwise, increase the Unique counter and use it to construct a new Var.
-}
makeVar :: (MonadSupply m, MonadScope m) => String -> m Var
makeVar name = do
    vars <- get
    case Map.lookup name vars of
        Just var -> pure var
        Nothing  -> do
            var <- freshVar name
            put $ Map.insert name var vars
            pure var

mkSomeUniVar :: forall f. T.Var -> T.SomeUniVar f
mkSomeUniVar var
    | '?':_ <- _varName var = Some $ T.UniVar Bool   var
    | '#':_ <- _varName var = Some $ T.UniVar Vector var
    | otherwise             = Some $ T.UniVar Field  var

{-| Type inference for variables
-}
inferUniVar
    :: forall m f. (MonadSupply m, MonadScope m) => R.Var -> m (T.SomeUniVar f)
inferUniVar = fmap mkSomeUniVar . makeVar . R.unVar

{-| Type inference for expressions
-}
inferExpr
    :: forall m f. (MonadTypeChecker m, TextField f)
    => R.Expr R.Var f -> m (T.SomeUniExpr f)
inferExpr (R.EConst (Some c@(T.UniConst uni _))) =
    pure $ SomeOf uni $ T.EConst c
inferExpr (R.EVar   v) = do
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
inferExpr (R.ETypeAnn u m) =
    case u of
        Some uni -> T.withKnownUni uni $ SomeOf uni <$> checkExpr m

{-| Mapping from Raw UnOp to Typed UnOp
-}
withTypedBinOp
    :: forall f r.
       R.BinOp
    -> (forall a b c. ( KnownUni f a, KnownUni f b, KnownUni f c)
        => T.BinOp f a b c -> r)
    -> r
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
withTypedUnOp
    :: forall f r.
       R.UnOp
    -> (forall a b. (KnownUni f a, KnownUni f b)
        => T.UnOp f a b -> r)
    -> r
withTypedUnOp R.Not  k = k T.Not
withTypedUnOp R.Neq0 k = k T.Neq0
withTypedUnOp R.Neg  k = k T.Neg
withTypedUnOp R.Inv  k = k T.Inv
withTypedUnOp R.Unp  k = k T.Unp

{-| Type checking for variables
-}
checkUniVar
    :: forall m f a. (MonadTypeChecker m)
    => Uni f a -> R.Var -> m (T.UniVar f a)
checkUniVar uni iden = do
    Some uniVar@(T.UniVar varUni _) <- inferUniVar iden
    let uniMismatch = typeMismatch uniVar uni varUni
    withGeqUniM uni varUni uniMismatch uniVar

{-| Type checking for expressions
-}
checkExpr
    :: forall m f a. (MonadTypeChecker m, TextField f, KnownUni f a)
    => R.Expr R.Var f -> m (T.Expr f a)
checkExpr (R.EIf l m n) = T.EIf <$> checkExpr l <*> checkExpr m <*> checkExpr n
checkExpr m = do
    SomeOf mUni tM <- inferExpr m
    let uni = knownUni @f @a
    let uniMismatch = typeMismatch tM uni mUni
    withGeqUniM uni mUni uniMismatch tM

checkProgram
    :: forall m f. (MonadTypeChecker m, TextField f)
    => R.Program R.Var f -> m (T.Program f)
checkProgram = traverse checkStatement


{-| Type checking judgement for statements of form
-}
checkStatement
    :: forall m f. (MonadTypeChecker m, TextField f)
    => R.Statement R.Var f -> m (T.Statement f)
checkStatement (R.ELet var m) = do
    Some uniVar@(T.UniVar uni _) <- inferUniVar var
    T.withKnownUni uni $ T.ELet uniVar <$> checkExpr m
checkStatement (R.EAssert m) =
    T.EAssert <$> checkExpr m
checkStatement (R.EFor var start end stmts) = do
    tVar <- makeVar $ R.unVar var
    T.EFor (T.UniVar Field tVar) start end <$> traverse checkStatement stmts

{-| Error message for a failed type equality
-}
typeMismatch
    :: forall a b c. (Show a, Show b, Show c)  => a -> b -> c -> TypeCheckError
typeMismatch expr expected found =
    [QQ.i|error: Universe mismatch for expression:
           #{show expr}
         Expected: #{show expected}
         Found:    #{show found}|]
