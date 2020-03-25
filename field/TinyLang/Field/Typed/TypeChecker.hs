{-# LANGUAGE QuasiQuotes #-}
{-| TypeChecker

Potential resources:
* https://www.cs.cmu.edu/~fp/courses/15312-f04/handouts/15-bidirectional.pdf
* http://www.cse.chalmers.se/~ulfn/papers/afp08/tutorial.pdf

-}

module TinyLang.Field.Typed.TypeChecker
    where

import           TinyLang.Prelude hiding (TypeError)

import           Data.Field
import           TinyLang.Environment
import           TinyLang.Var
import           TinyLang.ParseUtils
import           TinyLang.Field.Evaluator
import           TinyLang.Field.UniConst
import           TinyLang.Field.Existential
import qualified TinyLang.Field.Typed.Core  as T
import qualified TinyLang.Field.Raw.Core    as R

import           Data.Kind
import qualified Data.String.Interpolate    as QQ

{-| Utility Type Aliases or Constraints
-}
type TypeCheckError     = String
type MonadTypeError   m = MonadError TypeCheckError m
type MonadTypeChecker m = ( MonadSupply m
                          , MonadScope m
                          , MonadTypeError m
                          )

{-| TypeCheckerT Transformer
-}
newtype TypeCheckerT e (m :: Type -> Type) a =
    TypeChecker { runTypeCheckerT :: (ExceptT e (StateT Scope (SupplyT m))) a }
    deriving newtype (Monad, Functor, Applicative, MonadError e, MonadSupply, MonadScope)



{-| A simple type checker
-}
type TypeChecker = TypeCheckerT TypeCheckError Identity

{-|
-}
runTypeChecker :: TypeChecker a -> Either TypeCheckError a
runTypeChecker typeChecker =
    runIdentity $ runSupplyT
                $ evalStateT (runExceptT (runTypeCheckerT typeChecker))
                             mempty

{-|
-}
typeCheck
    :: forall f. (TextField f)
    => R.Expr R.Var f -> Either TypeCheckError (T.SomeUniExpr f)
typeCheck = runTypeChecker . inferExpr

{-|
-}
checkType
    :: forall f a. (TextField f, KnownUni f a)
    => R.Expr R.Var f -> Either TypeCheckError (T.Expr f a)
checkType = runTypeChecker . checkExpr (knownUni @f @a)

{-| 
-}
mkSomeUniVar :: forall f. T.Var -> T.SomeUniVar f
mkSomeUniVar var
    | '?':_ <- _varName var = Some $ T.UniVar Bool   var
    | '#':_ <- _varName var = Some $ T.UniVar Vector var
    | otherwise             = Some $ T.UniVar Field  var

{-| = Bidirectional Typing Rules
-}
inferUniVar
    :: forall m f. (MonadSupply m, MonadScope m) => R.Var -> m (T.SomeUniVar f)
inferUniVar = liftM mkSomeUniVar . makeVar . R.unVar

{-|
-}
inferExpr
    :: forall m f. (MonadTypeChecker m, TextField f)
    => R.Expr R.Var f -> m (T.SomeUniExpr f)
inferExpr (R.EConst (Some c@(T.UniConst uni _))) = pure $ SomeOf uni $ T.EConst c
inferExpr (R.EVar   v) = do
    Some uniVar@(T.UniVar uni _) <- inferUniVar v
    pure $ SomeOf uni $ T.EVar uniVar
inferExpr (R.EAppBinOp R.Or l m) = do
    tL <- checkExpr Bool l
    tM <- checkExpr Bool m
    pure $ SomeOf Bool $ T.EAppBinOp T.Or tL tM
inferExpr (R.EAppBinOp R.And l m) = do
    tL <- checkExpr Bool l
    tM <- checkExpr Bool m
    pure $ SomeOf Bool $ T.EAppBinOp T.And tL tM
inferExpr (R.EAppBinOp R.Xor l m) = do
    tL <- checkExpr Bool l
    tM <- checkExpr Bool m
    pure $ SomeOf Bool $ T.EAppBinOp T.Xor tL tM
inferExpr (R.EAppBinOp R.FEq l m) = do
    tL <- checkExpr Field l
    tM <- checkExpr Field m
    pure $ SomeOf Bool $ T.EAppBinOp T.FEq tL tM
inferExpr (R.EAppBinOp R.FLt l m) = do
    tL <- checkExpr Field l
    tM <- checkExpr Field m
    pure $ SomeOf Bool $ T.EAppBinOp T.FLt tL tM
inferExpr (R.EAppBinOp R.FLe l m) = do
    tL <- checkExpr Field l
    tM <- checkExpr Field m
    pure $ SomeOf Bool $ T.EAppBinOp T.FLe tL tM
inferExpr (R.EAppBinOp R.FGe l m) = do
    tL <- checkExpr Field l
    tM <- checkExpr Field m
    pure $ SomeOf Bool $ T.EAppBinOp T.FLe tL tM
inferExpr (R.EAppBinOp R.FGt l m) = do
    tL <- checkExpr Field l
    tM <- checkExpr Field m
    pure $ SomeOf Bool $ T.EAppBinOp T.FGt tL tM
inferExpr (R.EAppBinOp R.Add l m) = do
    tL <- checkExpr Field l
    tM <- checkExpr Field m
    pure $ SomeOf Field $ T.EAppBinOp T.Add tL tM
inferExpr (R.EAppBinOp R.Sub l m) = do
    tL <- checkExpr Field l
    tM <- checkExpr Field m
    pure $ SomeOf Field $ T.EAppBinOp T.Sub tL tM
inferExpr (R.EAppBinOp R.Mul l m) = do
    tL <- checkExpr Field l
    tM <- checkExpr Field m
    pure $ SomeOf Field $ T.EAppBinOp T.Mul tL tM
inferExpr (R.EAppBinOp R.Div l m) = do
    tL <- checkExpr Field l
    tM <- checkExpr Field m
    pure $ SomeOf Field $ T.EAppBinOp T.Div tL tM
inferExpr (R.EAppBinOp R.BAt l m) = do
    tL <- checkExpr Field  l
    tM <- checkExpr Vector m
    pure $ SomeOf Bool $ T.EAppBinOp T.BAt tL tM
inferExpr (R.EAppUnOp R.Not l) = do
    tL <- checkExpr Bool l
    pure $ SomeOf Bool $ T.EAppUnOp T.Not tL
inferExpr (R.EAppUnOp R.Neq0 l) = do
    tL <- checkExpr Field l
    pure $ SomeOf Bool $ T.EAppUnOp T.Neq0 tL
inferExpr (R.EAppUnOp R.Neg l) = do
    tL <- checkExpr Field l
    pure $ SomeOf Field $ T.EAppUnOp T.Neg tL
inferExpr (R.EAppUnOp R.Inv l) = do
    tL <- checkExpr Field l
    pure $ SomeOf Field $ T.EAppUnOp T.Inv tL
inferExpr (R.EAppUnOp R.Unp l) = do
    tL <- checkExpr Field l
    pure $ SomeOf Vector $ T.EAppUnOp T.Unp tL    
inferExpr (R.EStatement s l) = do
    tS <- checkStatement s
    SomeOf uni tL <- inferExpr l
    pure $ SomeOf uni $ flip (foldr T.EStatement) tS tL
inferExpr (R.EIf l m n) = do
    tL <- checkExpr Bool l
    SomeOf uni tM <- inferExpr m
    tN <- checkExpr uni n
    pure $ SomeOf uni $ T.EIf tL tM tN

{-|
-}
checkUniVar
    :: forall m f a. (MonadTypeChecker m) => Uni f a -> R.Var -> m (T.UniVar f a)
checkUniVar uni iden = do
    Some uniVar@(T.UniVar varUni _) <- inferUniVar iden
    let uniMismatch = typeMismatch uniVar uni varUni
    withGeqUniM uni varUni uniMismatch uniVar

{-|
-}
checkExpr :: forall m f a. (MonadTypeChecker m, TextField f) => Uni f a -> R.Expr R.Var f -> m (T.Expr f a)
checkExpr uni (R.EIf l m n) = do
    tL <- checkExpr Bool l
    tM <- checkExpr uni  m
    tN <- checkExpr uni  n
    pure $ T.EIf tL tM tN
checkExpr uni m = do
    SomeOf mUni tM <- inferExpr m
    let uniMismatch = typeMismatch tM uni mUni
    withGeqUniM uni mUni uniMismatch $ tM

{-|
-}
checkStatement
    :: forall m f. (MonadTypeChecker m, TextField f) => R.Statement R.Var f -> m [T.Statement f]
checkStatement (R.ELet var m) = do
    Some (uniVar@(T.UniVar uni _)) <- inferUniVar var
    tM <- checkExpr uni m
    pure . pure $ T.ELet uniVar tM
checkStatement (R.EAssert m) = do
    tM <- checkExpr Bool m
    pure . pure $ T.EAssert tM
checkStatement (R.EFor var start end stmts) = do
    tVar <- makeVar $ R.unVar var
    (unrollLoop tVar start end) . concat <$> mapM checkStatement stmts

{-|
-}
unrollLoop
    :: forall f. (TextField f) => T.Var -> Integer -> Integer -> [T.Statement f] -> [T.Statement f]
unrollLoop var lower bound stats = do
    i <- [lower .. bound]
    let env = insertVar var (Some $ fromInteger i) mempty
        report err = error $ "Panic: " ++ show err
    map (either report id . instStatement env) stats

{-|
-}
typeMismatch :: forall a b c. (Show a, Show b, Show c)  => a -> b -> c -> TypeCheckError
typeMismatch expr expected found =
    [QQ.i|error: Universe mismatch for expression:
           #{show expr}
         Expected: #{show expected}
         Found:    #{show found}|]
