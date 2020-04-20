module Data.Field
    ( Field (..)
    , AField (..)
    , ABaseField (..)
    , ToField (..)
    , TextField (..)
    , AsInteger (..)
    , two
    ) where

import           Prelude             hiding (div)
import qualified Prelude             (div)

import           Control.Exception   (ArithException (..), throw)
import           Control.Monad       (guard)
import qualified Data.Field.Galois   as GF
import           Data.Maybe          (fromMaybe)
import           Data.Proxy
import           Data.Ratio
import           GHC.TypeLits
import           Test.QuickCheck

infixl 6 `add`, `sub`
infixl 7 `mul`, `div`

-- There is a funny thing we can do: build a lazy tree like this:
--
--       0
--       +
--       1
--       +
--       2
--      + *
--     3   4
--    *   + *
--   6   5   8
--
-- (where @+@ means @+1@ and @*@ means @*2@.
--
-- This way we can efficiently convert integers to field elements.
-- And we can probably rebalance this tree in order to be able to efficiently perform search in it,
-- which would allow us to check whether a field element is in fact an integer and nicely print it
-- as such (especially useful when the field is Q).

-- We can also use previous elements in order to obtain new elements from them,
-- for example we can obtain @15@ using only addition like this:
--
-- 1 + 1 = 2
-- 2 + 1 = 3
-- 3 + 3 = 6
-- 6 + 6 = 12
-- 12 + 3 = 15

-- but this is known to be a computationally hard problem (see https://projecteuler.net/problem=122)
-- and we also need to use multiplication, so this is more of a Project Euler task than something
-- that we definitely need. Anyway, would be nice to have, just too much of a bother.

class Field f where
    {-# MINIMAL zer, add, one, mul, (neg | sub), (inv | div) #-}

    zer :: f

    neg :: f -> f
    neg x = zer `sub` x

    add :: f -> f -> f

    sub :: f -> f -> f
    x `sub` y = x `add` neg y

    one :: f

    inv :: f -> Maybe f
    inv x = one `div` x

    mul :: f -> f -> f

    div :: f -> f -> Maybe f
    x `div` y = mul x <$> inv y

makeInv :: (Eq f, Field f) => (f -> f) -> f -> Maybe f
makeInv i x = do
    -- Note that we assume that @i x@ can only fail when @x@ is zero.
    guard $ x /= zer
    Just $ i x

makeDiv :: (Eq f, Field f) => (f -> f -> f) -> f -> f -> Maybe f
makeDiv d x y = do
    -- Note that we assume that @x `d` y@ can only fail when @y@ is zero.
    guard $ y /= zer
    Just $ x `d` y

fromDivided :: Maybe f -> f
fromDivided = fromMaybe $ throw DivideByZero

class Field f => TextField f where
    -- TODO: use proper precedence-sensitive pretty-printing.
    showField :: f -> String
    default showField :: Show f => f -> String
    showField = show

newtype AField f = AField
    { unAField :: f
    } deriving (Eq, Functor, Foldable, Traversable)
      deriving newtype (Field, AsInteger, Arbitrary)

-- GHC will not derive this one automatically
instance (Field f, TextField f) => TextField (AField f) where
    showField  = showField . unAField

newtype ABaseField a = ABaseField
    { unABaseField :: a
    } deriving newtype (Eq, Num, Fractional)

instance (Eq a, Num a, Fractional a) => Field (ABaseField a) where
    zer = 0
    neg = negate
    add = (+)
    sub = (-)
    one = 1
    inv = makeInv recip
    mul = (*)
    div = makeDiv (/)

two :: Field f => f
two = one `add` one

deriving via ABaseField Rational instance Field Rational

instance Field Bool where
    zer = False
    neg = id
    add = (/=)
    one = True
    mul = (&&)

    inv False = Nothing
    inv True  = Just True

instance Field f => Num (AField f) where
    negate = neg
    (+)    = add
    (-)    = sub
    (*)    = mul
    abs    = error "no 'abs'"
    signum = error "no 'signum'"

    fromInteger n0
        | n0 >= 0   = go n0
        | otherwise = neg $ go (- n0)
        where
            go 0 = zer
            go 1 = one
            go 2 = two
            go n | even n = two `mul` fromInteger (n `Prelude.div` 2)
            go n = one `add` fromInteger (n - 1)

instance Field f => Fractional (AField f) where
    x / y = fromDivided $ x `div` y
    recip = fromDivided . inv
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance TextField f => Show (AField f) where
    show = showField . unAField

instance TextField Rational where
    showField r
        | denominator r == 1 = show $ numerator r
        | otherwise          = "(" ++ show (numerator r) ++ " / " ++ show (denominator r) ++ ")"

class Field f => ToField f a where
    toField :: a -> f

instance (Field f, f ~ f') => ToField f (AField f') where
    toField = unAField

instance Field f => ToField f Integer where
    toField = unAField . fromInteger

instance Field f => ToField f Bool where
    toField False = zer
    toField True  = one

instance Field f => ToField f Rational where
    toField = unAField . fromRational

{- | We're dealing with fields in which elements can be regarded as integers,
and we carry out comparisons via integers. The @AsInteger@ class adds an operation
which returns the integer corresponding to a field element.
-}
class AsInteger f where
    asInteger :: f -> Maybe Integer

-- | For Rational, we check if a fraction is in fact an integer.  We
-- can safely use the 'denominator' function to do this because it
-- reduces fractions to lowest terms before computing the result (eg,
-- denominator (111/3) == 1)
instance AsInteger Rational where
    asInteger r = do
        guard $ denominator r == 1
        Just $ numerator r

-- | Various instances making Data.Field.Galois.Prime fit our type
-- classes for fields.  Most of these would generalise to non-prime
-- fields quite easily, but parsing and printing would require some
-- work.

deriving via ABaseField (GF.Prime p) instance KnownNat p => Field (GF.Prime p)

instance KnownNat p => AsInteger (GF.Prime p) where
    asInteger x
        | i < p `Prelude.div` 2 = Just i
        | otherwise             = Just (- GF.fromP (neg x))
        where
            p = natVal @p Proxy
            i = GF.fromP x

instance KnownNat p => TextField (GF.Prime p) where
    -- 'asInteger' must always return a 'Just' for a 'GF.Prime'.
    showField = maybe (error "Panic: not an integer") show . asInteger
