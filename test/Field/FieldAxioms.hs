{-| QuickCheck tests for classes claiming to implement Field |-}
module Field.FieldAxioms
    (
     test_fields
    )
where

import           TinyLang.Field.Core
import           TinyLang.Field.F17
import           TinyLang.Field.F4913

import           Data.Proxy
import           Test.QuickCheck
--import           Test.Tasty
--import           Test.Tasty.QuickCheck

-- We need extra arguments of type Proxy f below so that we can tell tests which
-- type we want them to test: eg `quickCheck (prop_nonZeroInverse (Proxy :: Proxy Rational))`.

-- (f, add, zer, neg) is an abelian group
prop_addAssociative :: (Field f, Show f, Eq f) => Proxy f -> f -> f -> f -> Property
prop_addAssociative _ x y z = (x `add` y) `add` z === x `add` (y `add` z)

prop_addCommutative :: (Field f, Show f, Eq f) => Proxy f -> f -> f -> Property
prop_addCommutative _ x y = x `add` y === y `add` x

prop_zerAdditiveIdentity :: (Field f, Show f, Eq f) => Proxy f -> f -> Property
prop_zerAdditiveIdentity _ x = x `add` zer === x
-- We can get away with only doing this on one side because of
-- prop_addCommutative, and similarly for other tests.

prop_AdditiveInverse :: (Field f, Show f, Eq f) => Proxy f -> f -> Property
prop_AdditiveInverse _ x = x `add` (neg x) === zer


-- (f, mul, one) is a commutative monoid
prop_mulAssociative :: (Field f, Show f, Eq f) => Proxy f -> f -> f -> f -> Property
prop_mulAssociative _ x y z = (x `mul` y) `mul` z === x `mul` (y `mul` z)

prop_mulCommutative :: (Field f, Show f, Eq f) => Proxy f -> f -> f -> Property
prop_mulCommutative _ x y = x `mul` y === y `mul` x

prop_oneMultiplicativeIdentity :: (Field f, Show f, Eq f) => Proxy f -> f -> Property
prop_oneMultiplicativeIdentity _ x = x `mul` one === x -- && (one `mul` x === x)

-- nonzero elements have inverses
prop_nonZeroInverse :: (Field f, Show f, Eq f) => Proxy f -> f -> Property
prop_nonZeroInverse _ x = x /= zer ==> x `mul` (inv x) === one

-- mul distributes over add
prop_mulDistributive :: (Field f, Show f, Eq f) => Proxy f -> f -> f -> f -> Property
prop_mulDistributive _ x y z = x `mul` (y `add` z) === (x `mul` y) `add` (x `mul` z)

-- Strictly we should check that zer /= one as well.

-- If we knew the characteristic we might want to check that it was
-- either zero or prime, but we'd need a primality test for that.

-- Other properties like  0.x == 0  and  x /= 0 => (xa == xb => a == b)  follow from the axioms.


{- QuickCheck provides things like quickCheckAll which you can use to
   test all of the prop_... properties in a file.  It uses Template
   Haskell, and I don't think you can get it to apply each prop_ to a
   proxy before testing it. The runAll function below checks all of
   the properties one by one, which is maybe not ideal.

   I also tried using TypeApplications instead of proxies.  That made
   the individual properties nicer but I still couldn't get it to
   work with QuickCheck's TH stuff.
-}

runAllWithMaxSuccess :: (Field f, Show f, Eq f, Arbitrary f) => Int -> Proxy f -> IO ()
runAllWithMaxSuccess n proxy = do
  quickCheck (withMaxSuccess n (prop_addAssociative proxy))
  quickCheck (withMaxSuccess n (prop_addCommutative proxy))
  quickCheck (withMaxSuccess n (prop_zerAdditiveIdentity proxy))
  quickCheck (withMaxSuccess n (prop_AdditiveInverse proxy))
  quickCheck (withMaxSuccess n (prop_mulAssociative proxy))
  quickCheck (withMaxSuccess n (prop_mulCommutative proxy))
  quickCheck (withMaxSuccess n (prop_oneMultiplicativeIdentity proxy))
  quickCheck (withMaxSuccess n (prop_nonZeroInverse proxy))
  quickCheck (withMaxSuccess n (prop_mulDistributive proxy))

-- |Run all the tests, 10000 times each.  For example, `runAll (Proxy :: Proxy Rational)`.
-- The usual limit of 1000 might be a bit small for F4193: we might not hit the x=0
-- case of prop_nonZeroInverse for instance (though that doesn't really matter).
runAll :: (Field f, Show f, Eq f, Arbitrary f) => Proxy f -> IO ()
runAll proxy = runAllWithMaxSuccess 10000 proxy
               
-- |Test all of our favourite fields
test_fields :: IO ()
test_fields = do
  putStrLn "Checking field axioms for Rational"
  runAll @ Rational Proxy
  putStrLn "Checking field axioms for F17"
  runAll @ F17 Proxy
  putStrLn "Checking field axioms for F4913"
  runAll @ F4913 Proxy
