-- | QuickCheck tests for classes claiming to implement 'Field'

module Field.Axioms
    ( test_axiomsExamples
    ) where

import           TinyLang.Prelude

import           Data.Field
import           TinyLang.Field.NamedType

import           Test.Tasty
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
              x /= zer ==> inv x `mul` x === one
        , testPropertyHard "right" $ \(x :: AField f) ->
              x /= zer ==> x `mul` inv x === one
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

test_axioms :: TestableField f => NamedType f -> TestTree
test_axioms namedType =
    testGroup (coerce namedType)
        [ test_addAssociative            namedType
        , test_addCommutative            namedType
        , test_zerAdditiveIdentity       namedType
        , test_additiveInverse           namedType
        , test_mulAssociative            namedType
        , test_mulCommutative            namedType
        , test_oneMultiplicativeIdentity namedType
        , test_nonZeroInverse            namedType
        , test_mulDistributive           namedType
        ]

-- | Test all of our favourite fields
test_axiomsExamples :: TestTree
test_axiomsExamples =
    testGroup "axiomsExamples"
        [ test_axioms f17
        , test_axioms f4913
        , test_axioms rational
        ]
