{-# LANGUAGE UndecidableInstances #-}
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

import           TinyLang.Environment             as Env
import           TinyLang.Field.Evaluator
import           TinyLang.Field.Typed.Core
import           TinyLang.Field.Uni               (mkSomeUniVar, SomeUni)

-- import qualified Data.IntMap.Strict               as IntMap
import           Data.Kind
import qualified Data.Vector                      as Vector
import           QuickCheck.GenT
import           Test.QuickCheck                  (Arbitrary, Gen, arbitrary,
                                                   arbitrarySizedBoundedIntegral,
                                                   shrink, shrinkList, shrinkNothing,
                                                   sublistOf)
import           Test.QuickCheck.Instances.Vector ()


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

-- The next function is concerned with generating fresh variables
-- for use in let-expressions.  We generate names with a prefix from a-z
-- and with a fresh drawn from a supply.

genFreshUniVar :: forall f a m. (KnownUni f a, MonadGen m, MonadSupply m) => m (UniVar f a)
genFreshUniVar = do
    let uni = knownUni @f @a
    name <- elements $ map (:[]) ['a' .. 'z']
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


-- | A wrapper around @UniVar f a@ provided for its @Arbitrary@ instance that allows to generate
-- variables from the default set of them.

defaultIdents :: [(String, SomeUni f)]
defaultIdents = concat . transpose $ [fieldIdents, boolIdents, vectorIdents] where
        fieldIdents   = map (\ i -> (i, Some Field))  ["x", "y", "z", "p", "q", "r", "s", "t"]
        boolIdents    = map (\ i -> (i, Some Bool))   ["a", "b", "c", "d", "e", "f", "g", "h"]
        vectorIdents  = map (\ i -> (i, Some Vector)) ["q", "r", "s", "t", "u", "v", "w"]

newtype Default a = Default
    { unDefault :: a
    }
    deriving (Show, Eq, Functor, Foldable, Traversable)

-- type DefaultUniVar f a    = Default (UniVar f a)
-- type DefaultVars          = Default [Var]
type DefaultSomeUniVars f = Default (Vars f)

-- instance Arbitrary DefaultVars where
--     arbitrary = do
--         vars <- sublistOf $ defaultIdents
--         pure . Default $ runSupply $ traverse freshSomeUniVar vars
--         where
--         freshSomeUniVar (name, (Some uni)) = Some . UniVar uni <$> freshVar name

instance Arbitrary (DefaultSomeUniVars f) where
    arbitrary = do
        idents <- sublistOf $ defaultIdents
        pure . Default $ runSupply $ traverse freshSomeUniVar idents
        where freshSomeUniVar (name, someUni) = mkSomeUniVar someUni <$> freshVar name



-- | Generate a universe and feed it to the continuation.
withOneOfUnis :: MonadGen m => (forall a. KnownUni f a => Uni f a -> m b) -> m b
withOneOfUnis k = oneof [k Bool, k Field]

-- We define this as a separate function, because the @Arbitrary@ instance of @UniConst@ requires
-- @KnownUni f a@ and we do not need this constraint in the shrinker, which we reuse in the
-- @Arbitrary@ isntance of @SomeUniConst@.
-- | Shrink a 'UniConst'.
shrinkUniConst :: Arbitrary f => UniConst f a -> [UniConst f a]
shrinkUniConst (UniConst uni x) = map (UniConst uni) $ case uni of
    Bool   -> shrink x
    Field  -> shrink x
    Vector -> shrink x

instance (KnownUni f a, Field f, Arbitrary f) => Arbitrary (UniConst f a) where
    arbitrary =
        UniConst uni <$> case uni of
            Bool   -> arbitrary
            Field  -> frequency
                [ (1, fromIntegral <$> arbitrary @Int)
                , (1, fromIntegral <$> arbitrarySizedBoundedIntegral @Int)
                , (1, arbitrary)
                ]
            Vector -> arbitrary
        where
            uni = knownUni @f @a

    shrink = shrinkUniConst

instance (Field f, Arbitrary f) => Arbitrary (SomeUniConst f) where
    arbitrary = withOneOfUnis $ \(_ :: Uni f a) -> Some <$> arbitrary @(UniConst f a)

    shrink (Some uniConst) = Some <$> shrinkUniConst uniConst

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
withOneOfUnOps :: forall f b m r. (KnownUni f b, MonadGen m)
    => (forall a. KnownUni f a => UnOp f a b -> m r) -> m r
withOneOfUnOps k = oneof $ case knownUni @f @b of
    Bool   -> [k Not, k Neq0]
    Field  -> [k Neg, k Inv]
    Vector -> [k Unp]

-- | Generate a 'BinOp' and feed it to the continuation.
-- Note that @c@ is bound outside of the continuation and @a@ and @b@ are bound inside.
-- This means that the caller decides values of what type the generated operator must return,
-- but the caller does not care about the type of arguments and so we can pick any.
withOneOfBinOps :: forall f c m d. (Field f, Arbitrary f, KnownUni f c, KnownUni f d, MonadGen m)
    => (forall a b. (KnownUni f a, KnownUni f b) => BinOp f a b c -> m (Expr f d)) -> m (Expr f d)
withOneOfBinOps k = case knownUni @f @c of
    Bool   -> frequency $
        map ((,) 16) [k Or, k And, k Xor, k FEq] ++
        map ((,) 1)  [k FLt, k FLe, k FGe, k FGt, k BAt]
    Field  -> oneof [k Add, k Sub, k Mul, k Div]
    -- There are no binary operators that return a 'Vector' and hence we just generate an
    -- arbitrary constant vector here.
    Vector -> EConst <$> arbitraryM

-- | Generate a comparison operator and feed it to the continuation.
withOneOfComparisons
    :: forall f m r. MonadGen m
    => (BinOp f (AField f) (AField f) Bool -> m r) -> m r
withOneOfComparisons k = oneof [k FLt, k FLe, k FGe, k FGt]

-- | Generate a binary operator that can be turned into an assertion and feed it to the continuation.
withOneOfBinAsserts
    :: forall f m r. MonadGen m
    => (forall a. KnownUni f a => BinOp f a a Bool -> m r) -> m r
withOneOfBinAsserts k = oneof [k Or, k And, k Xor, k FEq, k FLt, k FLe, k FGe, k FGt]

-- | An arbitrary integer value (for use in comparisons)
arbitraryValI :: (Field f, MonadGen m) => m (UniConst f (AField f))
arbitraryValI = UniConst Field . fromInteger <$> arbitraryM

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

groundArbitraryFreqs :: (Field f, Arbitrary f, KnownUni f a, MonadGen m)
    => Vars f -> [(Int, m (Expr f a))]
groundArbitraryFreqs vars =
    -- NOTE:  check if there are available variables in the context
    case uniVars vars of
        [] -> [(1, EConst <$> arbitraryM)]
        uVars -> [ (1, EConst <$> arbitraryM)
                 , (2, EVar   <$> elements uVars)
                 ]

newtype SGenT f (m :: Type -> Type) a =
    SGen { unSGenT :: GenT (SupplyT (StateT (Vars f) m)) a }
    deriving newtype ( Monad
                     , Functor
                     , Applicative
                     , MonadSupply
                     , MonadState (Vars f)
                     , MonadGen
                     )

-- TODO:  Is there a way to automate this?
instance MonadState s m => MonadState s (GenT m) where
    get = lift get
    put = lift . put
    state = lift . state


type SGen f a = SGenT f Identity a

-- Not sure if this is the right type
runSGenT :: (Monad m) => Vars f -> SGenT f m a -> Gen (m a)
runSGenT vars = fmap ((`evalStateT` vars) . runSupplyT) . runGenT . unSGenT

runSGen :: Vars f -> SGen f a -> Gen a
runSGen vars = fmap runIdentity . runSGenT vars

boundedArbitraryStmts :: forall m f. (Field f, Arbitrary f, MonadGen m, MonadSupply m, MonadState (Vars f) m)
    => Int -> m (Statements f)
boundedArbitraryStmts size =
    Statements <$> frequency [ (1, pure [])
                               , (10, arbStmts)
                               ]
    where
        arbStmts = do
            numStmts :: Int <- choose (1, size)
            let perStmtSize = size `Prelude.div` numStmts
            resize numStmts $ listOf $ boundedArbitraryStmt perStmtSize

boundedArbitraryStmt :: forall m f. (Field f, Arbitrary f, MonadGen m, MonadSupply m, MonadState (Vars f) m)
    => Int -> m (Statement f)
boundedArbitraryStmt size
    | size <= 1 = do
          vars <- get
          EAssert <$> boundedArbitraryExpr vars size
    | otherwise = frequency stmtGens where
          stmtGens = [ (3, withOneOfUnis $ \(_ :: Uni f a') -> do
                               vars <- get
                               uniVar <- genFreshUniVar @f @a'
                               let vars' = Some uniVar : vars
                                   size' = size - 1
                               put vars'
                               ELet uniVar <$> boundedArbitraryExpr vars size')
                     -- Generate a completely random assertion (unlikely to hold)
                     , (1, do
                               vars <- get
                               EAssert <$> boundedArbitraryExpr vars size)
                     -- generate a valid (but necessarily holding) contraint
                     , (1, do
                                let size' = size - 1
                                vars <- get
                                EAssert <$> boundedArbitraryComparisons vars size')
                     -- generate an assertion of form @x binOp x@
                     , (1, withOneOfBinAsserts $ \binOp -> do
                               let size' = size `Prelude.div` 2
                               vars <- get
                               x <- boundedArbitraryExpr vars size'
                               pure $ EAssert $ EAppBinOp binOp x x)
                     ]

-- | Generate an expression of a particular type from a collection of variablesf
-- with the number of nodes (approximately) bounded by 'size'.
boundedArbitraryExpr :: forall m f a. (Field f, Arbitrary f, KnownUni f a, MonadGen m, MonadSupply m)
    => Vars f -> Int -> m (Expr f a)
boundedArbitraryExpr vars size
    -- TODO: A case when there are no variables in context
    | size <= 1 = frequency $ groundArbitraryFreqs vars
    | otherwise = frequency everything where
        everything = groundArbitraryFreqs vars ++ expressions ++ comparisons (size `Prelude.div` 2)

        -- The most general generator.
        expressions =
            [ (2, do
                    let size' = size `Prelude.div` 3
                    EIf
                        <$> boundedArbitraryExpr vars size'
                        <*> boundedArbitraryExpr vars size'
                        <*> boundedArbitraryExpr vars size')
            , (2, withOneOfUnOps  $ \unOp  -> do
                    let size' = size - 1
                    EAppUnOp unOp <$> boundedArbitraryExpr vars size')
            , (4, withOneOfBinOps $ \binOp -> do
                    let size' = size `Prelude.div` 2
                    EAppBinOp binOp
                        <$> boundedArbitraryExpr vars size'
                        <*> boundedArbitraryExpr vars size')
            ]

        -- A generator of comparisons.
        comparisons size' = case knownUni @f @a of
            Bool -> [(2, boundedArbitraryComparisons vars size')]
            _    -> []

boundedArbitraryComparisons :: (Field f, Arbitrary f, MonadGen m, MonadSupply m)
    => Vars f -> Int -> m (Expr f Bool)
boundedArbitraryComparisons vars size' =
    withOneOfComparisons $ \comp ->
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
boundedArbitraryExprI _    size | size <= 1 = EConst <$> arbitraryValI
boundedArbitraryExprI vars size             = frequency
    [ (1, EConst <$> arbitraryValI)
    -- , (0, EVar   <$> chooseUniVar vars)
      {- TODO:  Check for Haddock post lts-13.26
         NOTE.  If we allow variables here we won't generally know in
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
defaultUniConst :: forall f a. (KnownUni f a, Field f) => UniConst f a
defaultUniConst =
    UniConst uni $ case uni of
        Bool   -> True
        Field  -> 101
        Vector -> Vector.fromList [False, True, True, True, False, False, True]
    where
        uni = knownUni @f @a

-- TODO:  Not sure how to use it effecively yet
-- genExts :: Statements f -> [Var]
-- genExts stmts = runSupply $ traverse (freshVar . _varSigName) sigs where
--     sigs = elems $ progFreeVarSigs (Program mempty stmts)

-- NOTE: We only create closed programs for now
instance (Field f, Arbitrary f) => Arbitrary (Program f) where
    arbitrary = do
        exts  <- unDefault <$> arbitrary
        stmts <- runSGen exts $ sized $ \size -> do
                     adjustUniquesForVars exts
                     boundedArbitraryStmts size
        -- NOTE: we should not use arbitrary instance for Statements f directly,
        -- as it can generate a statement that contains free variables.
        pure $ Program exts stmts

    shrink (Program exts stmts) =
        fmap (uncurry Program) $
            [ (exts' , stmts)  | exts'  <- shrinkExts ] ++
            [ (exts  , stmts') | stmts' <- shrink stmts ] where
        shrinkExts = if Env.null unusedEnv then [] else
            [ sort (usedExts ++ shrunkUnusedExts)
                     | shrunkUnusedExts <- shrinkList shrinkNothing unusedExts ]
        toAscPair someUniVar@(Some (UniVar _ tVar)) = (tVar, someUniVar)
        oldEnv     = Env.fromVars $ map toAscPair exts
        newEnv     = progFreeVarSigs $ Program [] stmts
        usedExts   = Env.elems $ Env.intersection oldEnv newEnv
        unusedEnv  = Env.difference oldEnv newEnv
        unusedExts = Env.elems unusedEnv


instance (Field f, Arbitrary f) => Arbitrary (Statements f) where
    arbitrary = do
        vars <- unDefault <$> arbitrary
        let stmtsGen = sized $ \size -> do
                adjustUniquesForVars vars
                boundedArbitraryStmts size
        runSGen vars stmtsGen

    shrink (Statements stmts) =
            Statements <$> concat
                [shrunkEmpty, shrunkNorm, shrunkPreserving, shrunkNonPreserving] where

        shrunkEmpty :: [[Statement f]]
        shrunkEmpty = case stmts of
            [] -> []
            _  -> [[]]

        -- normalisation steps when shrinking
        shrunkNorm :: [[Statement f]]
        shrunkNorm = maybe [] pure $ norm stmts

        -- small step "normaliser"
        norm :: [Statement f] -> Maybe [Statement f]
        norm ((EAssert (EConst (UniConst _ True))) : restStmts) =
                pure restStmts
        norm (stmt : restStmts) = (stmt :) <$> norm restStmts
        -- no normalisations performed, abort
        norm [] = Nothing

        -- preserves the structure of statements
        shrunkPreserving :: [[Statement f]]
        shrunkPreserving = shrinkElements shrink stmts
        -- does not preserve the structure of statements
        shrunkNonPreserving :: [[Statement f]]
        shrunkNonPreserving = shrinkList shrink stmts

-- A modified shrinkList, that preserves the structure of the underlying list
shrinkElements :: (a -> [a]) -> [a] -> [[a]]
shrinkElements shr = shrinkOne where
  shrinkOne []     = []
  shrinkOne (x:xs) = [ x':xs | x'  <- shr x ]
                  ++ [ x:xs' | xs' <- shrinkOne xs ]

-- We do not provide an implementation for 'arbitrary' (because we don't need it and it'd be
-- annoying to write it), but we still want to make provide an 'Arbitrary' instance, so that
-- 'shrink' can be used in the 'Arbitrary' instance of 'Statments' (a separately provided
-- 'shrinkStatement' could also work).
instance (Field f, Arbitrary f) => Arbitrary (Statement f) where
    arbitrary = error "Panic: no implementation of 'arbitrary' for 'Statement'"

    shrink (ELet uniVar def) = withKnownUni (_uniVarUni uniVar) $ ELet uniVar <$> shrink def
    -- TODO: we need more clever shrinking here. E.g. in addition to normal shrinking
    -- (which most of the time will break the assertion) we should shrink @lhs == rhs@ to
    -- @lhs' == lhs'@ or @rhs' == rhs'@ where @lhs'@ and @rhs'@ are shrunk version of
    -- @lhs@ and @rhs@ respectively (just to have some shrinking that does not break the assertion).
    shrink (EAssert expr) = EAssert <$> shrink expr

instance (KnownUni f a, Field f, Arbitrary f) => Arbitrary (Expr f a) where
    arbitrary = runSupplyGenT . sized $ \size -> do
        vars <- liftGen $ unDefault <$> arbitrary
        adjustUniquesForVars vars
        boundedArbitraryExpr vars size

    -- TODO: also add @[SomeUniExpr f normed | normed /= expr, normed = normExpr env expr]@,
    -- but do not forget to catch exceptions.
    shrink (EConst uniConst) = EConst <$> shrink uniConst
    shrink expr0             = EConst defaultUniConst : case expr0 of
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
        EConst _ -> []
        EVar _ -> []


genEnvFromVarSigs :: (Field f, Arbitrary f) => Env (VarSig f) -> Gen (Env (SomeUniConst f))
genEnvFromVarSigs =
    traverse $ \(VarSig _ (uni :: Uni f a)) ->
        Some <$> withKnownUni uni (arbitrary :: Gen (UniConst f a))

genInputEnvFromExts :: (Field f, Arbitrary f) => Vars f -> Gen (Env (SomeUniConst f))
genInputEnvFromExts vars = fromVars . zip exts <$> consts where
    exts = map (\ (Some (UniVar _ v)) -> v) vars
    consts = for vars $ \ (Some (UniVar (uni :: Uni f a) _)) ->
                Some <$> withKnownUni uni (arbitrary :: Gen (UniConst f a))

-- | Generate a random ProgramWithEnv.  Note that you can say things like
-- "generate (resize 1000 arbitrary :: Gen (ProgramWithEnv F17))" to get bigger
-- expressions. There's no means provided to generate things over non-default
-- sets of variables, but this would be easy to do.
instance (Field f, Arbitrary f) => Arbitrary (ProgramWithEnv f) where
    arbitrary = do
        prog <- arbitrary
        vals <- genInputEnvFromExts . _programExts $ prog
        return $ ProgramWithEnv prog vals
    shrink (ProgramWithEnv prog vals) =
        flip map (shrink prog) $ \shrunk ->
            ProgramWithEnv shrunk . intersection vals $ progFreeVarSigs shrunk
