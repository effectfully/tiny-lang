module Data.Field
    ( Field (..)
    , AField (..)
    , AsInteger (..)
    , two
    ) where
    
import qualified GHC.Num          (fromInteger)
import           Prelude          hiding (div)
import qualified Prelude          (div)

import           Data.Coerce
import           Data.Ratio

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
-- that we definitely need. Anyway, would nice to have, just too much of a bother.

class Field f where
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

    {-# MINIMAL zer, add, one, mul, (neg | sub), (inv | div) #-}

newtype AField f = AField
    { unAField :: f
    } deriving (Eq)

two :: Field f => f
two = one `add` one

instance Field f => Field (AField f) where
    zer = coerce $ zer @f
    neg = coerce $ neg @f
    add = coerce $ add @f
    sub = coerce $ sub @f
    one = coerce $ one @f
    inv = coerce $ inv @f
    mul = coerce $ mul @f
    div = coerce $ div @f

instance Field Rational where
    zer = 0
    neg = negate
    add = (+)
    sub = (-)
    one = 1
    inv = \x -> denominator x % numerator x
    mul = (*)

instance Field f => Num (AField f) where
    negate = neg
    (+) = add
    (-) = sub
    (*) = mul

    fromInteger n0
        | n0 >= 0   = go n0
        | otherwise = neg $ go (- n0)
        where
            go 0          = zer
            go 1          = one
            go 2          = two
            go n | even n = two `mul` fromInteger (n `Prelude.div` 2)
            go n          = one `add` fromInteger (n - 1)

    abs    = error "no 'abs'"
    signum = error "no 'signum'"

instance Show f => Show (AField f) where
    show = show . unAField

class AsInteger f where
    asInteger :: f -> Maybe Integer

instance AsInteger f => AsInteger (AField f) where
    asInteger = coerce $ asInteger @f

instance AsInteger Rational where
    asInteger r = if denominator r == 1
                   then Just (numerator r)
                   else Nothing
                   
