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

module TinyLang.Field.Generator
where

import           TinyLang.Environment     (Env (..))
import           TinyLang.Field.Core
import           TinyLang.Field.Evaluator
import           TinyLang.Generator       ()
import           TinyLang.Var

import qualified Data.IntMap.Strict       as IntMap
import           Test.QuickCheck

-- Some stuff adapted from Vars.hs;  we need separate var names for
-- booleans and field elements, and boolean var names have to start with '?'
-- for the benefit of the parser.

type VarName = String

makeVars :: Int -> [VarName] -> [Var]
makeVars base = zipWith (\index name -> Var (Unique index) name) [base..]

defaultVarsF :: [Var]
defaultVarsF = makeVars 0 ["x", "y", "z", "p", "q", "r", "s", "t"]

-- Not terribly elegant
defaultVarsB :: [Var]
defaultVarsB = makeVars (length defaultVarsF)  ["?a", "?b", "?c", "?d", "?e", "?f", "?g", "?h"]

-- A pair of lists of vars for use by expression generators
data Vars = Vars
    { fieldVars :: [Var]
    , boolVars  :: [Var]
    }

defaultVars ::Vars
defaultVars = Vars defaultVarsF defaultVarsB

--  Generator for variables, choosing from the given list.
arbitraryVar :: [Var] -> Gen Var
arbitraryVar = elements

arbitraryEVarB :: [Var] -> Gen (Expr f Bool)
arbitraryEVarB vars = EVar . UniVar Bool <$> arbitraryVar vars

arbitraryEVarF :: [Var] -> Gen (Expr f (AField f))
arbitraryEVarF vars = EVar . UniVar Field <$> arbitraryVar vars

-- | An arbitrary integer value (for use in comparisons)
arbitraryValI :: Field f => Gen (UniVal f (AField f))
arbitraryValI = UniVal Field . fromInteger <$> arbitrary

instance (Arbitrary f, Field f) => Arbitrary (AField f) where
    arbitrary = AField <$> arbitrary

instance Arbitrary (UnOp f Bool Bool) where
    arbitrary = return Not

instance Arbitrary (UnOp f (AField f) Bool) where
    arbitrary = return Neq0

instance Arbitrary (UnOp f (AField f) (AField f)) where
    arbitrary = elements [Neg, Inv]

instance Arbitrary (BinOp f Bool Bool Bool) where
    arbitrary = elements [Or, And, Xor]

instance Arbitrary (BinOp f (AField f) (AField f) (AField f)) where
    arbitrary = elements [Add, Sub, Mul, Div]

-- We can compare any two elements of a field for equality.
arbitraryFFcomparison :: Gen (BinOp f (AField f) (AField f) Bool)
arbitraryFFcomparison = elements [FEq]

-- ... but we're only supposed to perform order comparisons on integer
-- values, so we need a separate generator for caomprison operations.
arbitraryIIcomparison :: Gen (BinOp f (AField f) (AField f) Bool)
arbitraryIIcomparison = elements [FLt, FLe, FGe, FGt]

instance Arbitrary (UniVal f Bool) where
    arbitrary = UniVal Bool <$> arbitrary

instance (Arbitrary f, Field f) => Arbitrary (UniVal f (AField f)) where
    arbitrary = UniVal Field <$> arbitrary

-- | An arbitrary boolean-valued expression
boundedAbritraryExprB :: (Field f, Arbitrary f) => Vars -> Int -> Gen (Expr f Bool)
boundedAbritraryExprB vars size =
    if size <= 1 then EVal <$> arbitrary
    else frequency [
              (1, EVal <$> arbitrary),
              (1, arbitraryEVarB (boolVars vars)),
              (2, EIf <$> boundedAbritraryExprB vars (size `Prelude.div` 3)
                      <*> boundedAbritraryExprB vars (size `Prelude.div` 3)
                      <*> boundedAbritraryExprB vars (size `Prelude.div` 3)),
              (2, EAppUnOp <$> arbitrary <*> boundedAbritraryExprB vars (size-1)),
              (2, EAppUnOp <$> arbitrary <*> boundedAbritraryExprF vars (size-1)),
              (2, EAppBinOp <$>
                arbitrary <*>
                boundedAbritraryExprB vars (size `Prelude.div` 2) <*>
                boundedAbritraryExprB vars (size `Prelude.div` 2)),
              (2, EAppBinOp <$>
                arbitraryFFcomparison <*>
                boundedAbritraryExprF vars (size `Prelude.div` 2) <*>
                boundedAbritraryExprF vars (size `Prelude.div` 2)),
              (2, EAppBinOp <$>
                arbitraryIIcomparison <*>
                usuallyIntValuedArbitraryExpr vars (size `Prelude.div` 2) <*>
                usuallyIntValuedArbitraryExpr vars (size `Prelude.div` 2))
                -- ^ We're only supposed to compare integer-valued
                -- expressions, but we generate occasional non-integer
                -- ones for testing purposes.
             ]

-- TODO: generate 'ELet's.
-- | An arbitrary field-valued expression
boundedAbritraryExprF :: (Field f, Arbitrary f) => Vars -> Int -> Gen (Expr f (AField f))
boundedAbritraryExprF vars size =
    if size <= 1 then EVal <$> arbitrary
    else frequency [
              (1, EVal <$> arbitrary),
              (1, arbitraryEVarF (fieldVars vars)),
              (3, EIf <$>
                boundedAbritraryExprB vars (size `Prelude.div` 3) <*>
                boundedAbritraryExprF vars (size `Prelude.div` 3) <*>
                boundedAbritraryExprF vars (size `Prelude.div` 3)),
              (3, EAppUnOp <$> arbitrary <*>  boundedAbritraryExprF vars (size-1)),
              (3, EAppBinOp <$>
                arbitrary <*>
                boundedAbritraryExprF vars (size `Prelude.div` 2) <*>
                boundedAbritraryExprF vars (size `Prelude.div` 2))
             ]

-- | Arbitrary unary operation for generating integer-valued
-- expressions.  We're disallowing Inv, so we only have negation.  Inv
-- would be OK in a finite field.
arbitraryUnOpRing :: Gen (UnOp f (AField f) (AField f))
arbitraryUnOpRing = elements [Neg]

-- | Arbitrary ring operation for generating integer-valued
-- expressions.  If we're in the rationals then division would usually
-- gice us non-integers, so / is omitted.  Note that if we're dealing
-- with a finite field then it's probably safe to allow / as well,
-- since we think that "integer" means something in the prime subfield
-- in that case, and that's closed under division (except for division
-- by zero).
arbitraryBinOpRing :: Gen (BinOp f (AField f) (AField f) (AField f))
arbitraryBinOpRing = elements [Add, Sub, Mul]

-- | This produces an arbitrary integer-valued expression.
-- Comparisons are only supposed to involve integers, so this
-- generates suitable arguments for them.  We've disallowed Inv and
-- Div, so we'll never get division by zero errors here.  The
-- expressions generated by this function don't include variables: see
-- the note below.
boundedAbritraryExprI :: (Field f, Arbitrary f) => Vars -> Int -> Gen (Expr f (AField f))
boundedAbritraryExprI vars size =
    if size <= 1 then EVal <$> arbitraryValI
    else frequency [
              (1, EVal <$> arbitraryValI),
              (0, arbitraryEVarF (fieldVars vars)),
              {- ^ NOTE.  If we allow variables here we won't generally know in
                 advance that they'll have integer values, so there
                 would be a danger that our comparisons will have a
                 high probability of failing.  We could fill the
                 environment with lots of integer-valued variables to
                 reduce the risk of this, or supply a separate list of
                 variables which we're certain will only contain integer
                 values.
               -}

              (3, EIf <$>
                boundedAbritraryExprB vars (size `Prelude.div` 3) <*>
                boundedAbritraryExprI vars (size `Prelude.div` 3) <*>
                boundedAbritraryExprI vars (size `Prelude.div` 3)),
              (3, EAppUnOp <$> arbitraryUnOpRing <*>  boundedAbritraryExprI vars (size-1)),
              (3, EAppBinOp <$>
                arbitraryBinOpRing <*>
                boundedAbritraryExprI vars (size `Prelude.div` 2) <*>
                boundedAbritraryExprI vars (size `Prelude.div` 2))
             ]

-- | Either a general field expression or one which is guaranteed to
-- be integer-valued.  We want the latter most of the time, but not
-- always.
usuallyIntValuedArbitraryExpr :: (Field f, Arbitrary f) => Vars -> Int -> Gen (Expr f (AField f))
usuallyIntValuedArbitraryExpr vars size =
    frequency [(1, boundedAbritraryExprF vars size),
               (9, boundedAbritraryExprI vars size)
              ]

-- Generate an expression from a collection of variables with the
-- number of nodes (approximately) bounded by 'size'
boundedAbritraryExpr :: (Field f, Arbitrary f) => Vars -> Int -> Gen (SomeUniExpr f)
boundedAbritraryExpr vars size = oneof [SomeUniExpr Bool <$> boundedAbritraryExprB vars size,
                                   (SomeUniExpr Field <$> (boundedAbritraryExprF vars size))]

-- Generate an expression over the default variables.  Again, this is bounded by 'size'.
defaultArbitraryExpr :: (Field f, Arbitrary f) => Int -> Gen (SomeUniExpr f)
defaultArbitraryExpr = boundedAbritraryExpr defaultVars

-- We can shrink any expression to just a hardcoded ground value (except we shouldn't shrink other
-- ground values to hardcoded ground values to prevent looping).
defaultUniVal :: forall f a. (KnownUni f a, Field f) => UniVal f a
defaultUniVal = case knownUni @f @a of
    Bool  -> UniVal Bool True
    Field -> UniVal Field $ fromInteger 100

instance (KnownUni f a, Field f, Arbitrary f) => Arbitrary (Expr f a) where
    arbitrary = case knownUni @f @a of
        Bool  -> sized $ boundedAbritraryExprB defaultVars
        Field -> sized $ boundedAbritraryExprF defaultVars

    shrink (EVal uniVal) = EVal <$> shrinkUniVal uniVal
    shrink expr0         = EVal defaultUniVal : case expr0 of
        EAppUnOp op e ->
            withKnownUni (uniOfUnOpArg op) $
                EAppUnOp op <$> shrink e
        EAppBinOp op e1 e2 ->
            withKnownUni uni1 $
            withKnownUni uni2 $
                uncurry (EAppBinOp op) <$> shrink (e1, e2)
          where
              (uni1, uni2) = uniOfBinOpArg op
        EIf e e1 e2 -> e1 : e2 : (uncurry (uncurry EIf) <$> shrink ((e, e1), e2))
        EVal _ -> []
        EVar _ -> []
        ELet uniVar def expr ->
            withKnownUni (_uniVarUni uniVar) $
                uncurry (ELet uniVar) <$> shrink (def, expr)

shrinkUniVal :: Arbitrary f => UniVal f a -> [UniVal f a]
shrinkUniVal (UniVal Bool b) = [UniVal Bool False | b]
shrinkUniVal (UniVal Field (AField i)) = map (UniVal Field . AField) $ shrink i

-- TODO: also add @[SomeUniExpr f normed | normed /= expr, normed = normExpr env expr]@,
-- but do not forget to catch exceptions.
shrinkSomeUniExpr
    :: (Field f, Arbitrary f) => Env (SomeUniVal f) -> SomeUniExpr f -> [SomeUniExpr f]
shrinkSomeUniExpr _ (SomeUniExpr uni0 expr) =
    map (SomeUniExpr uni0) (withKnownUni uni0 $ shrink expr) ++ case expr of
        EAppUnOp op e -> [SomeUniExpr (uniOfUnOpArg op) e]
        EAppBinOp op e1 e2 ->
            case uniOfBinOpArg op of
              (t1,t2) -> [SomeUniExpr t1 e1, SomeUniExpr t2 e2]
        EIf e _ _ -> [SomeUniExpr Bool e]
        EVal _ -> []
        EVar _ -> []
        ELet (UniVar uni _) def _ -> [SomeUniExpr uni def]

-- An instance that QuickCheck can use for tests.
instance (Field f, Arbitrary f) => Arbitrary (SomeUniExpr f)
    where arbitrary = sized defaultArbitraryExpr
          shrink = shrinkSomeUniExpr mempty

genUni :: Field f => Uni f a -> Gen a
genUni Bool  = arbitrary
genUni Field = fromInteger <$> arbitrary

genSomeUniVal :: Field f => Uni f a -> Gen (SomeUniVal f)
genSomeUniVal uni = SomeUniVal . UniVal uni <$> genUni uni

genEnvFromVarSigns :: Field f => Env (VarSign f) -> Gen (Env (SomeUniVal f))
genEnvFromVarSigns = traverse $ \(VarSign _ uni) -> genSomeUniVal uni

instance (Field f, Arbitrary f) => Arbitrary (SomeUniVal f)
    where arbitrary = oneof [SomeUniVal <$> (arbitrary :: Gen (UniVal f (AField f))),
                             SomeUniVal <$> (arbitrary :: Gen (UniVal f Bool))]

-- | Generate a random ExprWithEnv.  Note that you can say things like
-- "generate (resize 1000 arbitrary :: Gen (ExprWithEnv F17))" to get
-- bigger expressions.  There's no means provided to generate things
-- over non-default sets of variables, but this would be easy to do.
instance (Field f, Arbitrary f) => Arbitrary (ExprWithEnv f) where
    arbitrary = do
        expr <- arbitrary
        vals <- case expr of
            SomeUniExpr _ e -> genEnvFromVarSigns $ exprFreeVarSigns e
        return $ ExprWithEnv expr vals
    shrink (ExprWithEnv expr env@(Env vals)) =
        -- TODO: test me.
        flip map (shrinkSomeUniExpr env expr) $ \shrunk@(SomeUniExpr _ se) ->
            ExprWithEnv shrunk . Env . IntMap.intersection vals . unEnv $ exprFreeVarSigns se
