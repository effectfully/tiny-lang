module Data.Field
    ( Field (..)
    , AField (..)
    , ABaseField (..)
    , ToField (..)
    , TextField (..)
    , AsInteger (..)
    , IsNegative (..)
    , unsafeAsInteger
    , two
    ) where

import           Prelude          hiding (div)
import qualified Prelude          (div)

import           TinyLang.ParseUtils

import           Control.Exception (throw, ArithException (..))
import           Control.Monad     (guard)
import           Data.Maybe        (fromMaybe)
import           Data.Proxy
import           Data.Ratio
import           Data.Foldable     (asum)
import qualified Data.Field.Galois as GF
import           GHC.TypeLits
import           Text.Megaparsec
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

    inv :: f -> f
    inv x = one `div` x

    mul :: f -> f -> f

    div :: f -> f -> f
    x `div` y = x `mul` inv y

parseFieldDefault :: Field f => Parser f
parseFieldDefault = unAField . fromInteger <$> signedDecimal

-- Note that any value produced by 'showField' must be parsable by 'parseField' even if it appears
-- in a large expression. This is why we always pretty-print rationals in parens currently.
class Field f => TextField f where
    parseField :: Parser f
    default parseField :: Parser f
    parseField = parseFieldDefault

    -- TODO: use proper precedence-sensitive pretty-printing.
    showField :: f -> String
    default showField :: Show f => f -> String
    showField = show

newtype AField f = AField
    { unAField :: f
    } deriving (Eq, Functor, Foldable, Traversable)
      deriving newtype (Field, TextField, AsInteger, Arbitrary)

newtype ABaseField a = ABaseField
    { unABaseField :: a
    } deriving newtype (Num, Fractional)

instance (Num a, Fractional a) => Field (ABaseField a) where
    zer = 0
    neg = negate
    add = (+)
    sub = (-)
    one = 1
    mul = (*)
    div = (/)

two :: Field f => f
two = one `add` one

deriving via ABaseField Rational instance Field Rational

instance Field Bool where
    zer = False
    neg = id
    add = (/=)
    one = True
    mul = (&&)

    inv False = throw DivideByZero
    inv True  = True

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
            go 0          = zer
            go 1          = one
            go 2          = two
            go n | even n = two `mul` fromInteger (n `Prelude.div` 2)
            go n          = one `add` fromInteger (n - 1)

instance Field f => Fractional (AField f) where
    (/) = div
    recip = inv
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance TextField f => Show (AField f) where
    show = showField . unAField

-- Note that the parser for @Rational@ accepts ONLY plain integers. This is because we pretty-print
-- rationals as @a / b@ and so @/@ is parsed elsewhere as regular division.
instance TextField Rational where
    parseField = asum
        [ try (div <$> parseFieldDefault <* symbol "/" <*> parseFieldDefault)
        , parseFieldDefault
        , parens parseField
        ]

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

{- | We're dealing with fields in which certain elements can be regarded
 as integers, and we're only supposed to carry out comparisons on such
 elements.  In the case of a finite field, these are probably elements
 of the prime subfield.  The AsInteger class adds an operation which
 returns the actual integer corresponding to such an element, if there
 is such an integer.
-}
class AsInteger f where
    asInteger :: f -> Maybe Integer

unsafeAsInteger :: AsInteger f => f -> Integer
unsafeAsInteger = fromMaybe (throw Denormal) . asInteger

-- | For Rational, we check if a fraction is in fact an integer.  We
-- can safely use the 'denominator' function to do this because it
-- reduces fractions to lowest terms before computing the result (eg,
-- denominator (111/3) == 1)
instance AsInteger Rational where
    asInteger r = do
        guard $ denominator r == 1
        Just $ numerator r

-- | The 'IsNegative' class adds an operation that allows to check whether a field element is
-- negative. The class is currently used only for pretty-printing and its semantics allows for
-- treating an element of a finite field as being negative.
class IsNegative f where
    isNegative :: f -> Bool
    default isNegative :: (Ord f, Num f) => f -> Bool
    isNegative x = x < 0

instance IsNegative Rational

-- | Various instances making Data.Field.Galois.Prime fit our type
-- classes for fields.  Most of these would generalise to non-prime
-- fields quite easily, but parsing and printing would require some
-- work.

deriving via ABaseField (GF.Prime p) instance KnownNat p => Field (GF.Prime p)

instance KnownNat p => AsInteger (GF.Prime p) where
    asInteger = Just . GF.fromP

instance KnownNat p => IsNegative (GF.Prime p) where
    isNegative f = i > p `Prelude.div` 2 where
        p = natVal @p Proxy
        i = GF.fromP f

instance KnownNat p => TextField (GF.Prime p) where
    parseField = GF.toP <$> signedDecimal
    showField f
        | isNegative f = show $ i - p
        | otherwise    = show i
        where
            p = natVal @p Proxy
            i = GF.fromP f
