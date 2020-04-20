-- | QuickCheck tests for classes claiming to implement 'Field'

module Field.Axioms
    ( test_fields
    ) where

import           TinyLang.Prelude

import           Data.Field
import qualified TinyLang.Field.Jubjub    as Jubjub
import           TinyLang.Field.NamedType

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

{- Note [Arbitrary AField]
'(===)' expects something with a 'Show' instance, but we pretty-print field elements via a
different type class, so an additional wrapper is needed to provide an appropriate 'Show'
instance.
-}

type TestableField f = (TextField f, Eq f, Arbitrary f)

-- | So that tests are run 10000 times each. The usual limit of 1000 might be a bit small for
-- 'F4193': we might not hit the x=0 case of test_nonZeroInverse for instance
-- (though that doesn't really matter).
testPropertyHard :: Testable a => TestName -> a -> TestTree
testPropertyHard name = testProperty name . withMaxSuccess 10000

-- (f, add, zer, neg) is an abelian group
test_addAssociative :: forall f proxy. TestableField f => proxy f -> TestTree
test_addAssociative _ =
    testPropertyHard "addAssociative" $ \(x :: AField f) y z ->
        (x `add` y) `add` z === x `add` (y `add` z)

test_addCommutative :: forall f proxy. TestableField f => proxy f -> TestTree
test_addCommutative _ =
    testPropertyHard "addCommutative" $ \(x :: AField f) y ->
        x `add` y === y `add` x

test_zerAdditiveIdentity :: forall f proxy. TestableField f => proxy f -> TestTree
test_zerAdditiveIdentity _ =
    testGroup "zerAdditiveIdentity"
        [ testPropertyHard "left"  $ \(x :: AField f) ->
              zer `add` x === x
        , testPropertyHard "right" $ \(x :: AField f) ->
              x `add` zer === x
        ]

test_additiveInverse :: forall f proxy. TestableField f => proxy f -> TestTree
test_additiveInverse _ =
    testGroup "additiveInverse"
        [ testPropertyHard "left"  $ \(x :: AField f) ->
              neg x `add` x === zer
        , testPropertyHard "right" $ \(x :: AField f) ->
              x `add` neg x === zer
        ]

-- (f, mul, one) is a commutative monoid
test_mulAssociative :: forall f proxy. TestableField f => proxy f -> TestTree
test_mulAssociative _ =
    testPropertyHard "mulAssociative" $ \(x :: AField f) y z ->
        (x `mul` y) `mul` z === x `mul` (y `mul` z)

test_mulCommutative :: forall f proxy. TestableField f => proxy f -> TestTree
test_mulCommutative _ =
    testPropertyHard "mulCommutative" $ \(x :: AField f) y ->
        x `mul` y === y `mul` x

test_oneMultiplicativeIdentity :: forall f proxy. TestableField f => proxy f -> TestTree
test_oneMultiplicativeIdentity _ =
    testGroup "oneMultiplicativeIdentity"
        [ testPropertyHard "left"  $ \(x :: AField f) ->
              one `mul` x === x
        , testPropertyHard "right" $ \(x :: AField f) ->
              x `mul` one === x
        ]

-- nonzero elements have inverses
test_nonZeroInverse :: forall f proxy. TestableField f => proxy f -> TestTree
test_nonZeroInverse _ =
    testGroup "nonZeroInverse"
        [ testPropertyHard "left"  $ \(x :: AField f) ->
              case (`mul` x) <$> inv x of
                  Nothing  -> x   === zer
                  Just res -> res === 1
        , testPropertyHard "right" $ \(x :: AField f) ->
              case (x `mul`) <$> inv x of
                  Nothing  -> x   === zer
                  Just res -> res === one
        ]

-- mul distributes over add
test_mulDistributive :: forall f proxy. TestableField f => proxy f -> TestTree
test_mulDistributive _ =
    testPropertyHard "mulDistributive" $ \(x :: AField f) y z ->
        x `mul` (y `add` z) === (x `mul` y) `add` (x `mul` z)

-- Strictly we should check that zer /= one as well.

-- If we knew the characteristic we might want to check that it was
-- either zero or prime, but we'd need a primality test for that.

-- Other properties like  0.x == 0  and  x /= 0 => (xa == xb => a == b)  follow from the axioms.

-- | Test that any 'Int' can be converted to a field element and back, resulting in the same @Int@.
test_fromToInteger :: forall f proxy. (Field f, AsInteger f) => proxy f -> TestTree
test_fromToInteger _ =
    testPropertyHard "fromToInteger" $ \(i :: Large Int) ->
        let i' = fromIntegral i
        in asInteger (fromInteger i' :: AField f) == Just i'

-- | Test that multiplying two numbers in the field aligns with integer multiplication.
test_multiply :: forall f proxy. (Field f, AsInteger f) => proxy f -> TestTree
test_multiply _ =
    testPropertyHard "multiply" $ \(i :: Large Int, j :: Large Int) ->
         let i' = fromIntegral i
             j' = fromIntegral j
         in asInteger (fromInteger i' * fromInteger j' :: AField f) == Just (i' * j')

-- | Test that dividing by a number and then multiplying by the same number is identity.
test_divideMultiply :: forall f proxy. (Field f, AsInteger f) => proxy f -> TestTree
test_divideMultiply _ =
    testPropertyHard "divideMultiply" $ \(i :: Large Int, j :: Large Int) ->
         let i' = fromIntegral i
             j' = fromIntegral j
             x = fromInteger i' / fromInteger j' * fromInteger j' :: AField f
         in j' == 0 || asInteger x == Just i'

test_fromToIntegerJubjubBounds :: TestTree
test_fromToIntegerJubjubBounds =
    testCase "fromToIntegerJubjubBounds" $ do
        let mid = Jubjub.r `Prelude.div` 2
        for_ ([0, mid, -mid] >>= \i -> [i-1, i, i+1]) $ \i -> do
            -- Account for under/overflows.
            let res
                    | i < - (mid + 1) = i + Jubjub.r - 1
                    | i < mid         = i
                    | otherwise       = i - Jubjub.r
            asInteger (fromInteger i :: AField Jubjub.F) @?= Just res

test_axioms :: TestableField f => proxy f -> TestTree
test_axioms proxy =
    testGroup "axioms"
        [ test_addAssociative            proxy
        , test_addCommutative            proxy
        , test_zerAdditiveIdentity       proxy
        , test_additiveInverse           proxy
        , test_mulAssociative            proxy
        , test_mulCommutative            proxy
        , test_oneMultiplicativeIdentity proxy
        , test_nonZeroInverse            proxy
        , test_mulDistributive           proxy
        ]

-- | Test all of our favourite fields
test_fields :: TestTree
test_fields =
    testGroup "fields"
        [ testGroup "F17"
            [ test_axioms f17
            ]
        , testGroup "Rational"
            [ test_axioms rational
            , test_fromToInteger rational
            , test_multiply rational
            , test_divideMultiply rational
            ]
        , testGroup "Jubjub.F"
            [ test_axioms jubjubF
            , test_fromToInteger jubjubF
            , test_multiply jubjubF
            , test_divideMultiply jubjubF
            , test_fromToIntegerJubjubBounds
            ]
        ]
