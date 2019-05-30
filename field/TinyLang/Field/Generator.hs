{-# OPTIONS_GHC -fno-warn-orphans #-}

module TinyLang.Field.Generator
where

import TinyLang.Environment ()
import TinyLang.Field.Core
import TinyLang.Generator ()
import TinyLang.Field.Printer ()

import qualified Data.IntMap.Strict   as IntMap ()
import           Test.QuickCheck


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

instance (Arbitrary (AField f)) => Arbitrary (UniVal f (AField f)) where
    arbitrary = UniVal Field  <$> arbitrary

boundedAbritraryExpr_B :: forall f. (Field f, Arbitrary f) => Int -> Gen (Expr f Bool)
boundedAbritraryExpr_B size =
    if size <= 1 then EVal <$> arbitrary
    else frequency [
              (1, EVal <$> arbitrary),
              (2, EIf <$> boundedAbritraryExpr_B (size `Prelude.div` 3)
                      <*> boundedAbritraryExpr_B (size `Prelude.div` 3)
                      <*> boundedAbritraryExpr_B (size `Prelude.div` 3)),
              (2, EAppUnOp <$> arbitrary <*> boundedAbritraryExpr_B (size-1)),
              (2, EAppUnOp <$> arbitrary <*> boundedAbritraryExpr_F (size-1)),
              (2, EAppBinOp <$>
                arbitrary <*>
                boundedAbritraryExpr_B (size `Prelude.div` 2) <*>
                boundedAbritraryExpr_B (size `Prelude.div` 2)),
              (2, EAppBinOp <$>
                arbitrary <*>
                boundedAbritraryExpr_F (size `Prelude.div` 2) <*>
                boundedAbritraryExpr_F (size `Prelude.div` 2))
             ]

boundedAbritraryExpr_F :: forall f. (Field f, Arbitrary f) => Int -> Gen (Expr f (AField f))
boundedAbritraryExpr_F size =
    if size <= 1 then EVal <$> arbitrary
    else frequency [
              (1, EVal <$> (arbitrary :: Gen (UniVal f (AField f)))),
              (3, EIf <$>
                boundedAbritraryExpr_B (size `Prelude.div` 3) <*>
                boundedAbritraryExpr_F (size `Prelude.div` 3) <*>
                boundedAbritraryExpr_F (size `Prelude.div` 3)),
              (3, EAppUnOp <$> arbitrary <*>  boundedAbritraryExpr_F (size-1)),
              (3, EAppBinOp <$>
                arbitrary <*>
                boundedAbritraryExpr_F (size `Prelude.div` 2) <*>
                boundedAbritraryExpr_F (size `Prelude.div` 2))
             ]

-- boundedAbritraryExpr :: forall f a. (Arbitrary (AField f)) => Int -> Gen (Expr f a)
-- boundedAbritraryExpr size = oneof [boundedAbritraryExpr_B size, boundedAbritraryExpr_F size]
