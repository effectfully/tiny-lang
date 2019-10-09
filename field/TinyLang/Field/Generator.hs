{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | NOTE: comparisons.

   We're now allowing comparisons of field elements with the operators
   <, <=, >=, and >.  We're only supposed to compare things which have
   "integer" values, and then the comparison is on the corresponding
   integers.  In this context, "integer" means (we think) some integer
   multiple of 1.  In the characteristic zero case this will mean a
   genuine integer, and in the characteristic p case it will mean an
   element of the prime subfield, which is isomorphic to Z_p.

   This has involved adding new generators for integer-valued things,
   and these have names ending with 'I' below.  The Num instance of
   Field contains a fromInteger function which produces integer values
   as binary expansions in terms of 'one'.  For a proper finite field
   implementation fromInteger would probably have a much more direct
   implementation.  In the evaluator we use fields equipped with an
   'asInteger' operation which converts in the opposite direction so
   we can actually perform comparisons of integers.
-}

module TinyLang.Field.Generator where

import           TinyLang.Prelude

import           TinyLang.Field.Core
import           TinyLang.Field.Evaluator

import qualified Data.IntMap.Strict        as IntMap
import qualified Data.Vector               as Vector
import           QuickCheck.GenT
import           Test.QuickCheck           hiding (elements, frequency, oneof,
                                                   sized)
import           Test.QuickCheck.Instances.Vector ()

deriving newtype instance Arbitrary f => Arbitrary (AField f)

-- Our generators all run in such an @m@ that @MonadGen m@ and
-- @MonadSupply m@ are satisfied for it, so that we can generate fresh
-- variables. The final @Arbitrary@ instances call @runSupplyGenT@ to get
-- back into the @Gen@ monad so that the types are right for QuickCheck.

-- Some stuff adapted from Vars.hs;  we need separate var names for
-- booleans and field elements, and boolean var names have to start with '?'
-- for the benefit of the parser.

-- TODO: move me somewhere else.
instance MonadSupply m => MonadSupply (GenT m)

arbitraryM :: (MonadGen m, Arbitrary a) => m a
arbitraryM = liftGen arbitrary

runSupplyGenT :: GenT Supply a -> Gen a
runSupplyGenT = fmap runSupply . runGenT

-- | A heterogeneous list of variables.
type Vars f = [SomeUniVar f]

-- | Extract variables that live in a particular universe.
uniVars :: forall f a. KnownUni f a => Vars f -> [UniVar f a]
uniVars =
    mapMaybe $ forget $ \uniVar@(UniVar uni _) ->
        withGeqUni uni (knownUni @f @a) Nothing $ Just uniVar

-- | Choose a variable of a particular type.
chooseUniVar :: (KnownUni f a, MonadGen m) => Vars f -> m (UniVar f a)
chooseUniVar = elements . uniVars

-- The next function is concerned with generating fresh variables
-- for use in let-expressions.  We generate names with a prefix from a-z
-- and with a fresh drawn from a supply.

genFreshUniVar :: forall f a m. (KnownUni f a, MonadGen m, MonadSupply m) => m (UniVar f a)
genFreshUniVar = do
    let uni = knownUni @f @a
    name <- elements $ case uni of
        Bool   -> map (\c -> '?':[c]) ['a'..'z']
        Field  -> map (:[]) ['a'..'z']
        Vector -> map (\c -> '#':[c]) ['a'..'z']
    UniVar uni <$> freshVar name

{- The next function does some hacking to adjust the supply of
   Uniques.  When we want to generate a new variable for a let
   expression we need to call freshUnique.  Unfortunately the lists of
   default variables we have here have Uniques the are generated
   outside the Supply monad, and if we just start calling freshUnique
   we'll generate new Uniques that clash with the ones in the default
   variables.  To avoid this there's a function 'adjustUniquesForVars'
   which finds the largest unique in a collection of variables and
   then sets the current free unique so that the Supply monad is in a
   state where any new calls to freshUnique will generate uniques that
   we haven't already used.  You then have to call
   adjustUniquesForVars before running the generator.

   Presumably to do this properly we'd need to do
   all name generation in the Supply monad, but then I don't think
   we'd be able to get our Arbitrary instances to have the right types
   (???).
-}

-- Call this inside the exposed generators!
adjustUniquesForVars :: MonadSupply m => Vars f -> m ()
adjustUniquesForVars =
    supplyFromAtLeast . freeUniqueFoldable . map (forget $ _varUniq . _uniVarVar)

-- | The variables used by our generators by default.
defaultVars :: Vars f
defaultVars = runSupply $ do
    let make uni name = Some . UniVar uni <$> freshVar name
    fieldVars  <- traverse (make Field ) ["x", "y", "z", "p", "q", "r", "s", "t"]
    boolVars   <- traverse (make Bool  ) ["?a", "?b", "?c", "?d", "?e", "?f", "?g", "?h"]
    vectorVars <- traverse (make Vector) ["#q", "#r", "#s", "#t", "#u", "#v", "#w"]
    return $ fieldVars ++ boolVars ++ vectorVars

-- | A wrapper around @UniVar f a@ provided for its @Arbitrary@ instance that allows to generate
-- variables from the default set of them.
newtype DefaultUniVar f a = DefaultUniVar
    { unDefaultUniVar :: UniVar f a
    }

instance KnownUni f a => Arbitrary (DefaultUniVar f a) where
    arbitrary = DefaultUniVar <$> chooseUniVar defaultVars

-- | Generate a universe and feed it to the continuation.
withOneofUnis :: MonadGen m => (forall a. KnownUni f a => Uni f a -> m b) -> m b
withOneofUnis k = oneof [k Bool, k Field]

-- We define this as a separate function, because the @Arbitrary@ instance of @UniVal@ requires
-- @KnownUni f a@ and we do not need this constraint in the shrinker, which we reuse in the
-- @Arbitrary@ isntance of @SomeUniVal@.
-- | Shrink a 'UniVal'.
shrinkUniVal :: Arbitrary f => UniVal f a -> [UniVal f a]
shrinkUniVal (UniVal uni x) = map (UniVal uni) $ case uni of
    Bool   -> shrink x
    Field  -> shrink x
    Vector -> shrink x

instance (KnownUni f a, Field f, Arbitrary f) => Arbitrary (UniVal f a) where
    arbitrary =
        UniVal uni <$> case uni of
            Bool   -> arbitrary
            Field  -> frequency
                [ (1, fromIntegral <$> arbitrary @Int)
                , (1, fromIntegral <$> arbitrarySizedBoundedIntegral @Int)
                , (1, arbitrary)
                ]
            Vector -> arbitrary
        where
            uni = knownUni @f @a

    shrink = shrinkUniVal

instance (Field f, Arbitrary f) => Arbitrary (SomeUniVal f) where
    arbitrary = withOneofUnis $ \(_ :: Uni f a) -> Some <$> arbitrary @(UniVal f a)

    shrink (Some uniVal) = Some <$> shrinkUniVal uniVal

{- When we've generated a fresh variable v for an expression let v =
   e1 in e2, we allow the textual name of v to be equal to the textual name of
   some other variable, but the unique of v must be different from the uniques of
   other variables. This means that we occasionally generate terms that is impossible
   to get by parsing, but we might get such terms after compiling from a high-level
   language, so it's good to test this scenario.
-}

-- TODO.  It's quite hard to get expressions 'let x=e1 in e2' where x
-- is actually used in e2.  We can turn up the frequency of production
-- of EVar expressions, but if we do that then we tend to get small
-- terms most of the time.  Maybe try making local variables more
-- popular?

-- | Generate an 'UnOp' and feed it to the continuation.
-- Note that @b@ is bound outside of the continuation and @a@ is bound inside.
-- This means that the caller decides values of what type the generated operator must return,
-- but the caller does not care about the type of argument and so we can pick any.
withOneofUnOps
    :: forall f b m r. (KnownUni f b, MonadGen m)
    => (forall a. KnownUni f a => UnOp f a b -> m r) -> m r
withOneofUnOps k = oneof $ case knownUni @f @b of
    Bool   -> [k Not, k Neq0]
    Field  -> [k Neg, k Inv]
    Vector -> [k Unp]

-- | Generate a 'BinOp' and feed it to the continuation.
-- Note that @c@ is bound outside of the continuation and @a@ and @b@ are bound inside.
-- This means that the caller decides values of what type the generated operator must return,
-- but the caller does not care about the type of arguments and so we can pick any.
withOneofBinOps
    :: forall f c m d. (Field f, Arbitrary f, KnownUni f c, KnownUni f d, MonadGen m)
    => (forall a b. (KnownUni f a, KnownUni f b) => BinOp f a b c -> m (Expr f d)) -> m (Expr f d)
withOneofBinOps k = case knownUni @f @c of
    Bool   -> frequency $
        map ((,) 16) [k Or, k And, k Xor, k FEq] ++
        map ((,) 1)  [k FLt, k FLe, k FGe, k FGt, k BAt]
    Field  -> oneof [k Add, k Sub, k Mul, k Div]
    -- There are no binary operators that return a 'Vector' and hence we just generate an
    -- arbitrary constant vector here.
    Vector -> EVal <$> arbitraryM

-- | Generate a comparison operator and feed it to the continuation.
withOneofComparisons
    :: forall f m r. MonadGen m
    => (BinOp f (AField f) (AField f) Bool -> m r) -> m r
withOneofComparisons k = oneof [k FLt, k FLe, k FGe, k FGt]

-- | Generate a binary operator that can be turned into an assertion and feed it to the continuation.
withOneofBinAsserts
    :: forall f m r. MonadGen m
    => (forall a. KnownUni f a => BinOp f a a Bool -> m r) -> m r
withOneofBinAsserts k = oneof [k Or, k And, k Xor, k FEq, k FLt, k FLe, k FGe, k FGt]

-- | An arbitrary integer value (for use in comparisons)
arbitraryValI :: (Field f, MonadGen m) => m (UniVal f (AField f))
arbitraryValI = UniVal Field . fromInteger <$> arbitraryM

-- | Arbitrary unary operation for generating integer-valued
-- expressions.  We're disallowing Inv, so we only have negation.  Inv
-- would be OK in a finite field.
arbitraryUnOpRing :: MonadGen m => m (UnOp f (AField f) (AField f))
arbitraryUnOpRing = elements [Neg]

-- | Arbitrary ring operation for generating integer-valued
-- expressions.  If we're in the rationals then division would usually
-- gice us non-integers, so / is omitted.  Note that if we're dealing
-- with a finite field then it's probably safe to allow / as well,
-- since we think that "integer" means something in the prime subfield
-- in that case, and that's closed under division (except for division
-- by zero).
arbitraryBinOpRing :: MonadGen m => m (BinOp f (AField f) (AField f) (AField f))
arbitraryBinOpRing = elements [Add, Sub, Mul]

groundArbitraryFreqs
    :: (Field f, Arbitrary f, KnownUni f a, MonadGen m)
    => Vars f -> [(Int, m (Expr f a))]
groundArbitraryFreqs vars =
    [ (1, EVal <$> arbitraryM)
    , (2, EVar <$> chooseUniVar vars)
    ]

-- | Generate an expression of a particular type from a collection of variables
-- with the number of nodes (approximately) bounded by 'size'.
boundedArbitraryExpr
    :: (Field f, Arbitrary f, KnownUni f a, MonadGen m, MonadSupply m)
    => Vars f -> Int -> m (Expr f a)
boundedArbitraryExpr vars0 size0 = go vars0 size0 where
    go :: forall f a m. (Field f, Arbitrary f, KnownUni f a, MonadGen m, MonadSupply m)
       => Vars f -> Int -> m (Expr f a)
    go vars size | size <= 1 = frequency $ groundArbitraryFreqs vars
    go vars size             = frequency everything where
        everything = groundArbitraryFreqs vars ++ recursive ++ comparisons (size `Prelude.div` 2)

        -- The most general generator.
        recursive =
            [ (2, do
                    let size' = size `Prelude.div` 3
                    EIf
                        <$> go vars size'
                        <*> go vars size'
                        <*> go vars size')
            , (4, withOneofUnis $ \(_ :: Uni f a') -> do
                    uniVar <- genFreshUniVar @f @a'
                    let vars' = Some uniVar : vars
                        size' = size `Prelude.div` 2
                    EStatement . ELet uniVar
                        <$> go vars  size'
                        <*> go vars' size')
            , (2, withOneofUnOps  $ \unOp  -> do
                    let size' = size - 1
                    EAppUnOp unOp <$> go vars size')
            , (4, withOneofBinOps $ \binOp -> do
                    let size' = size `Prelude.div` 2
                    EAppBinOp binOp
                        <$> go vars size'
                        <*> go vars size')
            , (round $ fromIntegral size / fromIntegral size0 * (4 :: Double), frequency
                  [ (4, do
                        -- Generates valid (but not necessarily holding) range constraints.
                        let size' = size `Prelude.div` 3
                        EStatement . EAssert
                            <$> boundedArbitraryComparisons vars size'
                            <*> go vars size')
                  , (4, withOneofBinAsserts $ \binOp -> do
                        -- Generates assertions of the @x op x@ form.
                        let size' = size `Prelude.div` 2
                        x <- go vars size'
                        EStatement (EAssert $ EAppBinOp binOp x x)
                            <$> go vars size')
                  , (4, do
                        let size' = size `Prelude.div` 2
                        -- Generates assertions that are unlikely to hold.
                        EStatement . EAssert
                            <$> go vars size'
                            <*> go vars size')
                  ])
            ]

        -- A generator of comparisons.
        comparisons size' = case knownUni @f @a of
            Bool -> [(2, boundedArbitraryComparisons vars size')]
            _    -> []

boundedArbitraryComparisons
    :: (Field f, Arbitrary f, MonadGen m, MonadSupply m)
    => Vars f -> Int -> m (Expr f Bool)
boundedArbitraryComparisons vars size' =
    withOneofComparisons $ \comp ->
        EAppBinOp comp
            <$> boundedArbitraryExprI vars size'
            <*> boundedArbitraryExprI vars size'

-- | This produces an arbitrary integer-valued expression.
-- Comparisons are only supposed to involve integers, so this
-- generates suitable arguments for them.  We've disallowed Inv and
-- Div, so we'll never get division by zero errors here.  The
-- expressions generated by this function don't include variables: see
-- the note below.
boundedArbitraryExprI
    :: (Field f, Arbitrary f, MonadGen m, MonadSupply m)
    => Vars f -> Int -> m (Expr f (AField f))
boundedArbitraryExprI _    size | size <= 1 = EVal <$> arbitraryValI
boundedArbitraryExprI vars size             = frequency
    [ (1, EVal <$> arbitraryValI)
    , (0, EVar <$> chooseUniVar vars)
      {- ^ NOTE.  If we allow variables here we won't generally know in
         advance that they'll have integer values, so there
         would be a danger that our comparisons will have a
         high probability of failing.  We could fill the
         environment with lots of integer-valued variables to
         reduce the risk of this, or supply a separate list of
         variables which we're certain will only contain integer
         values.
         This Note also applies to the @size <= 1@ case above.
       -}
    , (2, do
            let size' = size `Prelude.div` 3
            EIf
                <$> boundedArbitraryExpr  vars size'
                <*> boundedArbitraryExprI vars size'
                <*> boundedArbitraryExprI vars size')
    , (2, do
            uniVar <- genFreshUniVar
            let vars' = Some uniVar : vars
                size' = size `Prelude.div` 2
            EStatement . ELet uniVar
                <$> boundedArbitraryExprI vars size'
                <*> boundedArbitraryExprI vars' size')
    , (2, do
            let size' = size - 1
            EAppUnOp
                <$> arbitraryUnOpRing
                <*> boundedArbitraryExprI vars size')
    , (2, do
            let size' = size `Prelude.div` 2
            EAppBinOp
                <$> arbitraryBinOpRing
                <*> boundedArbitraryExprI vars size'
                <*> boundedArbitraryExprI vars size')
    ]

{- Note [Shrinking]
We have two shrinkers: one that preserves types (i.e. acts on @Expr f a@) and the other one that
doesn't (i.e. acts on @SomeExpr f@). The latter shrinker calls the former shrinker and so whenever
shrinking can be done in a type-preserving way, only the type-preserving shrinker implements that.
E.g. in a non-type-preserving shrinker we explicitly shrink

    ELet (UniVar uni _) def _ -> []

only to

    SomeUniExpr uni def

even though shrinking to the body of the let-expression would also be correct, but since such
shrinking is type-preserving, we let the type-preserving shrinker do it.
-}

-- We can shrink any expression to just a hardcoded ground value (except we shouldn't shrink other
-- ground values to hardcoded ground values to prevent looping).
defaultUniVal :: forall f a. (KnownUni f a, Field f) => UniVal f a
defaultUniVal =
    UniVal uni $ case uni of
        Bool   -> True
        Field  -> fromInteger 101
        Vector -> Vector.fromList [False, True, True, True, False, False, True]
    where
        uni = knownUni @f @a

-- We do not provide an implementation for 'arbitrary' (because we don't need it and it'd be
-- annoying to write it), but we still want to make provide an 'Arbitrary' instance, so that
-- 'shrink' can be used in the 'Arbitrary' instance of 'Expr' (a separately provided
-- 'shrinkStatement' wouldn't work, because we want to shrink a pair of values, see the instance).
instance (Field f, Arbitrary f) => Arbitrary (Statement f) where
    arbitrary = error "No implementation of 'arbitrary' for 'Statement'"

    shrink (ELet uniVar def) = withKnownUni (_uniVarUni uniVar) $ ELet uniVar <$> shrink def
    -- TODO: we need more clever shrinking here. E.g. in addition to normal shrinking
    -- (which most of the time will break the assertion) we should shrink @lhs == rhs@ to
    -- @lhs' == lhs'@ or @rhs' == rhs'@ where @lhs'@ and @rhs'@ are shrunk version of
    -- @lhs@ and @rhs@ respectively (just to have some shrinking that does not break the assertion).
    shrink (EAssert expr)    = EAssert <$> shrink expr

instance (KnownUni f a, Field f, Arbitrary f) => Arbitrary (Expr f a) where
    arbitrary = runSupplyGenT . sized $ \size -> do
        let vars = defaultVars
        adjustUniquesForVars vars
        boundedArbitraryExpr vars size

    -- TODO: also add @[SomeUniExpr f normed | normed /= expr, normed = normExpr env expr]@,
    -- but do not forget to catch exceptions.
    shrink (EVal uniVal) = EVal <$> shrink uniVal
    shrink expr0         = EVal defaultUniVal : case expr0 of
        EAppUnOp op e ->
            withUnOpUnis op $ \uni _ ->
            withKnownUni uni $
                EAppUnOp op <$> shrink e
        EAppBinOp op e1 e2 ->
            withBinOpUnis op $ \uni1 uni2 _ ->
            withKnownUni uni1 $
            withKnownUni uni2 $
                uncurry (EAppBinOp op) <$> shrink (e1, e2)
        EIf e e1 e2 -> e1 : e2 : (uncurry (uncurry EIf) <$> shrink ((e, e1), e2))
        EVal _ -> []
        EVar _ -> []
        -- TODO: we can safely drop an assertion and we can drop a let-expression when
        -- the let-bound variable is not used in @expr@.
        EStatement stat expr -> uncurry EStatement <$> shrink (stat, expr)

-- An instance that QuickCheck can use for tests.
instance (Field f, Arbitrary f) => Arbitrary (SomeUniExpr f) where
    arbitrary = withOneofUnis $ \uni -> SomeUniExpr uni <$> arbitrary

    shrink (SomeUniExpr uni0 expr) =
        map (SomeUniExpr uni0) (withKnownUni uni0 $ shrink expr) ++ case expr of
            EAppUnOp op e -> withUnOpUnis op $ \argUni _ -> [SomeUniExpr argUni e]
            EAppBinOp op e1 e2 ->
                withBinOpUnis op $ \uni1 uni2 _ ->
                    [SomeUniExpr uni1 e1, SomeUniExpr uni2 e2]
            EIf e _ _ -> [SomeUniExpr Bool e]
            EVal _ -> []
            EVar _ -> []
            EStatement stat _ -> case stat of
                ELet (UniVar uni _) def -> [SomeUniExpr uni def]
                EAssert e               -> [SomeUniExpr Bool e]

genEnvFromVarSigns :: (Field f, Arbitrary f) => Env (VarSign f) -> Gen (Env (SomeUniVal f))
genEnvFromVarSigns =
    traverse $ \(VarSign _ (uni :: Uni f a)) ->
        Some <$> withKnownUni uni (arbitrary :: Gen (UniVal f a))

-- | Generate a random ExprWithEnv.  Note that you can say things like
-- "generate (resize 1000 arbitrary :: Gen (ExprWithEnv F17))" to get
-- bigger expressions.  There's no means provided to generate things
-- over non-default sets of variables, but this would be easy to do.
instance (Field f, Arbitrary f) => Arbitrary (ExprWithEnv f) where
    arbitrary = do
        someUniExpr <- arbitrary
        vals <- case someUniExpr of
            SomeUniExpr _ expr -> genEnvFromVarSigns $ exprFreeVarSigns expr
        return $ ExprWithEnv someUniExpr vals
    shrink (ExprWithEnv someUniExpr (Env vals)) =
        -- TODO: test me.
        flip map (shrink someUniExpr) $ \shrunk@(SomeUniExpr _ expr) ->
            ExprWithEnv shrunk . Env . IntMap.intersection vals . unEnv $ exprFreeVarSigns expr
