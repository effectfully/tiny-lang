{-# OPTIONS_GHC -fno-warn-orphans #-}

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
arbitraryEVarB vars = EVar Bool <$> arbitraryVar vars

arbitraryEVarF :: [Var] -> Gen (Expr f (AField f))
arbitraryEVarF vars = EVar Field <$> arbitraryVar vars


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

instance Arbitrary (BinOp f (AField f) (AField f) Bool) where
    arbitrary = elements [FEq]

instance Arbitrary (BinOp f (AField f) (AField f) (AField f)) where
    arbitrary = elements [Add, Sub, Mul, Div]

instance Arbitrary (UniVal f Bool) where
    arbitrary = UniVal Bool <$> arbitrary

instance (Arbitrary f, Field f) => Arbitrary (UniVal f (AField f)) where
    arbitrary = UniVal Field  <$> arbitrary


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
                arbitrary <*>
                boundedAbritraryExprF vars (size `Prelude.div` 2) <*>
                boundedAbritraryExprF vars (size `Prelude.div` 2))
             ]

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

-- Generate an expression from a collection of variables with the
-- number of nodes (approximately) bounded by 'size'
boundedAbritraryExpr :: (Field f, Arbitrary f) => Vars -> Int -> Gen (SomeUniExpr f)
boundedAbritraryExpr vars size = oneof [SomeUniExpr Bool <$> boundedAbritraryExprB vars size,
                                   (SomeUniExpr Field <$> (boundedAbritraryExprF vars size))]

-- Generate an expression over the default variables.  Again, this is bounded by 'size'.
defaultArbitraryExpr :: (Field f, Arbitrary f) => Int -> Gen (SomeUniExpr f)
defaultArbitraryExpr = boundedAbritraryExpr defaultVars

uniOfUnOpArg :: UnOp f a b -> Uni f a
uniOfUnOpArg Not  = Bool
uniOfUnOpArg Neq0 = Field
uniOfUnOpArg Inv  = Field
uniOfUnOpArg Neg  = Field

unisOfBinOpArg :: BinOp f a b c ->  (Uni f a, Uni f b)
unisOfBinOpArg Or  = (Bool, Bool)
unisOfBinOpArg And = (Bool, Bool)
unisOfBinOpArg Xor = (Bool, Bool)
unisOfBinOpArg FEq = (Field, Field)
unisOfBinOpArg Add = (Field, Field)
unisOfBinOpArg Sub = (Field, Field)
unisOfBinOpArg Mul = (Field, Field)
unisOfBinOpArg Div = (Field, Field)

shrinkUniVal :: Arbitrary f => UniVal f a -> [UniVal f a]
shrinkUniVal (UniVal Bool b) = [UniVal Bool False | b]
shrinkUniVal (UniVal Field (AField i)) = map (UniVal Field . AField) $ shrink i

-- TODO: also add @[SomeUniExpr f normed | normed /= expr, normed = normExpr env expr]@,
-- but do not forget to catch exceptions.
shrinkExpr :: Arbitrary f => Env (SomeUniVal f) -> SomeUniExpr f -> [SomeUniExpr f]
shrinkExpr _ (SomeUniExpr f expr) =
    case expr of
      EAppUnOp op e -> [SomeUniExpr (uniOfUnOpArg op) e]
      EAppBinOp op e1 e2 ->
          case unisOfBinOpArg op of
            (t1,t2) -> [SomeUniExpr t1 e1, SomeUniExpr t2 e2]
      EIf e e1 e2 -> [SomeUniExpr Bool e, SomeUniExpr f e1, SomeUniExpr f e2]
      EVal uniVal -> SomeUniExpr f . EVal <$> shrinkUniVal uniVal
      EVar _ _ -> []

-- An instance that QuickCheck can use for tests.
instance (Field f, Arbitrary f) => Arbitrary (SomeUniExpr f)
    where arbitrary = sized defaultArbitraryExpr
          shrink = shrinkExpr mempty

genUni :: Field f => Uni f a -> Gen a
genUni Bool  = arbitrary
genUni Field = fromInteger <$> arbitrary

genSomeUniVal :: Field f => Uni f a -> Gen (SomeUniVal f)
genSomeUniVal uni = SomeUniVal . UniVal uni <$> genUni uni

genEnvFromVarSigns :: Field f => IntMap.IntMap (VarSign f) -> Gen (Env (SomeUniVal f))
genEnvFromVarSigns = fmap Env . traverse (\(VarSign _ uni) -> genSomeUniVal uni)

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
        vals <- case expr of SomeUniExpr _ e -> genEnvFromVarSigns (exprVarSigns e)
        return $ ExprWithEnv expr vals
    shrink (ExprWithEnv expr env@(Env vals)) =
        -- TODO: test me.
        flip map (shrinkExpr env expr) $ \shrunkExpr@(SomeUniExpr _ se) ->
            ExprWithEnv shrunkExpr . Env . IntMap.intersection vals $ exprVarSigns se
