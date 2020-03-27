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
    , typeCheck
    , checkType
    , inferExpr
    , checkExpr
    , inferUniVar
    , checkUniVar
    )
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
checkType = runTypeChecker . checkExpr


{-| = Bidirectional Typing Rules

Here is a quick recap of our current syntax of the language

@
identifiers         i
variables           x       ::= i | ?i | #i
boolean constants   B       ::= T | F
field constants     F       ::= /determined by the choice of field/
vector constants    V       ::= { B+ }
integer constants   I
constants           c       ::= B | F | V
universes           U       ::= Bool | Field | Vector
statements          S, T    ::= let x = M | assert M
                              | for var = I to I do S* end
expressions         L, M, N ::= c | x | L binOp M | unOp L | L [ M ] | S ; L
                              | if L then M else N
binary operations   binOp   ::= or | ...
unary operations    unOp    ::= not | ...
@

We assume the followin metafunctions:

* tyConst(c), which returns the type of a constant,

* tyVar(x), which returns the type of a variable,

* tyLeft(binOp), which returns the type of the first argument of binOp,

* tyRight(binOp), which returns the type of the second argument of binOp,

* tyResult(binOp), which returns the type of the result of binOp,

* tyArg(unOp), which returns the type of the argument of unOp,

* tyResult(unOp), which returns the type of the result of unOp.

We split the type-checking into the following judgements:

* Type checking judgement which checks the type of expression, for
  which both the expression @M@ and the universe @U@ are inputs:
  @ M^+ <= U^+ @

* Type synthesis (inference) judgement which infers the type of the
  expression, for which the expression @M@ is an input and the
  universe @U@ is an output:
  @ M^+ => U^- @

* Type equality judgement which checks wether two universes match, for
  which both the universe @U1@ and @U2@ are inputs:
  @ U1^+ = U2^+ @

We can switch from type checking judgement to type inference judgement
using type equality

@
M => U2
U2 = U1
-------
M <= U1
@

Please note that at the moment we lack type ascriptions and for
convenience we add type inference rule for if-then-else expressions.

-}

mkSomeUniVar :: forall f. T.Var -> T.SomeUniVar f
mkSomeUniVar var
    | '?':_ <- _varName var = Some $ T.UniVar Bool   var
    | '#':_ <- _varName var = Some $ T.UniVar Vector var
    | otherwise             = Some $ T.UniVar Field  var

{-| Type inference for variables
-}
inferUniVar
    :: forall m f. (MonadSupply m, MonadScope m) => R.Var -> m (T.SomeUniVar f)
inferUniVar = liftM mkSomeUniVar . makeVar . R.unVar

{-| Type inference for expressions
@
M => U
@
-}
inferExpr
    :: forall m f. (MonadTypeChecker m, TextField f)
    => R.Expr R.Var f -> m (T.SomeUniExpr f)
{-|
@
---------------
c => tyConst(c)
@
-}
inferExpr (R.EConst (Some c@(T.UniConst uni _))) =
    pure $ SomeOf uni $ T.EConst c
{-|
@
-------------
x => tyVar(x)
@
-}    
inferExpr (R.EVar   v) = do
    Some uniVar@(T.UniVar uni _) <- inferUniVar v
    pure $ SomeOf uni $ T.EVar uniVar
{-|
@
L <= tyLeft(binOp)
M <= tyRight(binOp)
----------------------------
L binOp M => tyResult(binOp)
@
-}    
inferExpr (R.EAppBinOp rBinOp l m) =
    withTypedBinOp rBinOp $
    \tBinOp ->
        SomeOf knownUni <$> (T.EAppBinOp tBinOp <$> checkExpr l <*> checkExpr m)
{-|
@
L <= tyArg(unOp)
------------------------
unOp L => tyResult(unOp)
@
-}        
inferExpr (R.EAppUnOp rUnOp l) =
    withTypedUnOp rUnOp $
    \tUnOp ->
        SomeOf knownUni <$> (T.EAppUnOp tUnOp <$> checkExpr l)
{-|
@
|- S
L => U
----------
S ; L => U
@
-}        
inferExpr (R.EStatement s l) = do
    tS <- checkStatement s
    SomeOf uni tL <- inferExpr l
    pure $ SomeOf uni $ flip (foldr T.EStatement) tS tL
{-|
@
|- L <= Bool
|- M => U
|- N <= U
--------------------------
|- if L then M else N => U
@
-}    
inferExpr (R.EIf l m n) = do
    tL <- checkExpr l
    SomeOf uni tM <- inferExpr m
    tN <- T.withKnownUni uni $ checkExpr n
    pure $ SomeOf uni $ T.EIf tL tM tN

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

{-| Type checking for expressions of the form
@
|- M <= U
@
-}
checkExpr
    :: forall m f a. (MonadTypeChecker m, TextField f, KnownUni f a)
    => R.Expr R.Var f -> m (T.Expr f a)
{-|
@
L <= Bool
M <= U
N <= U
--------------------------
if L then M else N <= U
@
-}
checkExpr (R.EIf l m n) = T.EIf <$> checkExpr l <*> checkExpr m <*> checkExpr n
{-|
@
M => U2
U1 = U2
-------
M <= U1
@
-}
checkExpr m = do
    SomeOf mUni tM <- inferExpr m
    let uni = knownUni @f @a
    let uniMismatch = typeMismatch tM uni mUni
    withGeqUniM uni mUni uniMismatch $ tM

{-| Type checking judgement for statements of form
@
|- S
@
-}
checkStatement
    :: forall m f. (MonadTypeChecker m, TextField f)
    => R.Statement R.Var f -> m [T.Statement f]
{-|
@
M <= tyVar(x)
-------------
|- let x = M
@
-}
checkStatement (R.ELet var m) = do
    Some (uniVar@(T.UniVar uni _)) <- inferUniVar var
    tM <- T.withKnownUni uni $ checkExpr m
    pure . pure $ T.ELet uniVar tM
{-|
@
M <= Bool
-----------
|- assert M
@
-}
checkStatement (R.EAssert m) = do
    tM <- checkExpr m
    pure . pure $ T.EAssert tM
{-|
@
|- \vec{S}
----------------------------------
|- for var = I to I do \vec{S} end
@
-}
checkStatement (R.EFor var start end stmts) = do
    tVar <- makeVar $ R.unVar var
    (unrollLoop tVar start end) . concat <$> mapM checkStatement stmts

{-| Statically unroll for statement loop
-}
unrollLoop
    :: forall f. (TextField f)
    => T.Var -> Integer -> Integer -> [T.Statement f] -> [T.Statement f]
unrollLoop var lower bound stats = do
    i <- [lower .. bound]
    let env = insertVar var (Some $ fromInteger i) mempty
        report err = error $ "Panic: " ++ show err
    map (either report id . instStatement env) stats

{-| Error message for a failed type equality
-}
typeMismatch
    :: forall a b c. (Show a, Show b, Show c)  => a -> b -> c -> TypeCheckError
typeMismatch expr expected found =
    [QQ.i|error: Universe mismatch for expression:
           #{show expr}
         Expected: #{show expected}
         Found:    #{show found}|]
