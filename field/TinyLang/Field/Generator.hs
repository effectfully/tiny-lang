{-# OPTIONS_GHC -fno-warn-orphans #-}

module TinyLang.Field.Generator
where

import           TinyLang.Environment   (Env (..))
import           TinyLang.Field.Core
import           TinyLang.Field.F17
import           TinyLang.Field.Printer ()
import           TinyLang.Generator     ()
import           TinyLang.Var


import qualified Data.IntMap.Strict     as IntMap
import           Test.QuickCheck

-- Some stuff adapted from Vars.hs;  we need separate var names for
-- booleans and field elements, and boolean var names have to start with '?'
-- for the benefit of the parser.

type VarName = String

makeVars :: Int -> [VarName] -> [Var]
makeVars base = zipWith (\index name -> Var (Unique index) name) [base..]

defaultVars_F :: [Var]
defaultVars_F = makeVars 0 ["x", "y", "z", "p", "q", "r", "s", "t"]

-- Not terribly elegant
defaultVars_B :: [Var]
defaultVars_B = makeVars (length defaultVars_F)  ["?a", "?b", "?c", "?d", "?e", "?f", "?g", "?h"]

-- A pair of lists of vars for use by expression generators
data Vars = Vars {fieldVars::[Var], boolVars::[Var]}

defaultVars::Vars
defaultVars = Vars defaultVars_F defaultVars_B

--  Generator for variables, choosing from the given list.
arbitraryVar :: [Var] -> Gen Var
arbitraryVar = elements

arbitraryEVar_B :: [Var] -> Gen (Expr f Bool)
arbitraryEVar_B vars = EVar Bool <$> (arbitraryVar vars)

arbitraryEVar_F :: [Var] -> Gen (Expr f (AField f))
arbitraryEVar_F vars = EVar Field <$> (arbitraryVar vars)


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


boundedAbritraryExpr_B :: (Field f, Arbitrary f) => Vars -> Int -> Gen (Expr f Bool)
boundedAbritraryExpr_B vars size =
    if size <= 1 then EVal <$> arbitrary
    else frequency [
              (1, EVal <$> arbitrary),
              (2, arbitraryEVar_B (boolVars vars)),
              (2, EIf <$> boundedAbritraryExpr_B vars (size `Prelude.div` 3)
                      <*> boundedAbritraryExpr_B vars (size `Prelude.div` 3)
                      <*> boundedAbritraryExpr_B vars (size `Prelude.div` 3)),
              (2, EAppUnOp <$> arbitrary <*> boundedAbritraryExpr_B vars (size-1)),
              (2, EAppUnOp <$> arbitrary <*> boundedAbritraryExpr_F vars (size-1)),
              (2, EAppBinOp <$>
                arbitrary <*>
                boundedAbritraryExpr_B vars (size `Prelude.div` 2) <*>
                boundedAbritraryExpr_B vars (size `Prelude.div` 2)),
              (2, EAppBinOp <$>
                arbitrary <*>
                boundedAbritraryExpr_F vars (size `Prelude.div` 2) <*>
                boundedAbritraryExpr_F vars (size `Prelude.div` 2))
             ]

boundedAbritraryExpr_F :: (Field f, Arbitrary f) => Vars -> Int -> Gen (Expr f (AField f))
boundedAbritraryExpr_F vars size =
    if size <= 1 then EVal <$> arbitrary
    else frequency [
              (1, EVal <$> arbitrary),
              (2, arbitraryEVar_F (fieldVars vars)),
              (3, EIf <$>
                boundedAbritraryExpr_B vars (size `Prelude.div` 3) <*>
                boundedAbritraryExpr_F vars (size `Prelude.div` 3) <*>
                boundedAbritraryExpr_F vars (size `Prelude.div` 3)),
              (3, EAppUnOp <$> arbitrary <*>  boundedAbritraryExpr_F vars (size-1)),
              (3, EAppBinOp <$>
                arbitrary <*>
                boundedAbritraryExpr_F vars (size `Prelude.div` 2) <*>
                boundedAbritraryExpr_F vars (size `Prelude.div` 2))
             ]

-- Generate an expression from a collection of variables with the
-- number of nodes (approximately) bounded by 'size'
boundedAbritraryExpr :: (Field f, Arbitrary f) => Vars -> Int -> Gen (SomeUniExpr f)
boundedAbritraryExpr vars size = oneof [SomeUniExpr Bool <$> boundedAbritraryExpr_B vars size,
                                   (SomeUniExpr Field <$> (boundedAbritraryExpr_F vars size))]

-- Generate an expression over the default variables.  Again, this is bounded by 'size'.
defaultArbitraryExpr :: (Field f, Arbitrary f) => Int -> Gen (SomeUniExpr f)
defaultArbitraryExpr = boundedAbritraryExpr defaultVars

-- An instance that QuickCheck can use for tests.
instance (Field f, Arbitrary f) => Arbitrary (SomeUniExpr f)
    where arbitrary = sized defaultArbitraryExpr

{-
-- A simple shrinker (could be improved)
shrinkExpr :: Expr f a -> [Expr f a]
shrinkExpr (EAppUnOp _ e)      = [e]
shrinkExpr (EAppBinOp _ e1 e2) = [e1, e2]
shrinkExpr (EIf e e1 e2)       = [e, e1, e2]
shrinkExpr (EVal _)            = []  -- Can't shrink an atom
shrinkExpr (EVar _ _)          = []
-}



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

data ExprWithEnv f a
    = ExprWithEnv (SomeUniExpr f) (Env (SomeUniVal f))
      deriving (Show)

instance (Field f, Arbitrary f) => Arbitrary (ExprWithEnv f a) where
    arbitrary = do
        expr <- arbitrary
        vals <- case expr of SomeUniExpr _ e -> genEnvFromVarSigns (exprVarSigns e)
        return $ ExprWithEnv expr vals

