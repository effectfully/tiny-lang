module Data.Field.F17
    ( F17 (unF17)
    , toF17
    ) where

import           Data.Field
import           Data.Hashable
import           Test.QuickCheck

{-| A quick implementation of the field of seventeen elements for
  testing.  We just wrap integers in a newtype and perform operations
  modulo 17, with inverses hard-coded in a table. |-}

newtype F17 = F17
    { unF17 :: Int
    } deriving (Eq)
      deriving newtype (Hashable)
      deriving anyclass (TextField)

-- Always use mod, not rem! Rem gives the wrong result for negative values.
toF17 :: Int -> F17
toF17 i = F17 $ i `mod` 17

instance Show F17 where
    show (F17 n) = show n

instance Field F17 where
    zer = F17 0
    one = F17 1
    add (F17 m) (F17 n) = toF17 $ m + n
    sub (F17 m) (F17 n) = toF17 $ m - n
    mul (F17 m) (F17 n) = toF17 $ m * n
    inv (F17 n) = F17 <$> case n of
        0  -> Nothing
        1  -> Just 1
        2  -> Just 9
        3  -> Just 6
        4  -> Just 13
        5  -> Just 7
        6  -> Just 3
        7  -> Just 5
        8  -> Just 15
        9  -> Just 2
        10 -> Just 12
        11 -> Just 14
        12 -> Just 10
        13 -> Just 4
        14 -> Just 11
        15 -> Just 8
        16 -> Just 16
        _  -> error $ "Panic: F17 is not in the [0, 16] range: " ++ show n

-- | Z_17 is a prime field, so every element can be obtained canonically from an integer.
instance AsInteger F17 where
    asInteger = Just . fromIntegral . unF17

-- | Choose a random element uniformly. If you use this in
-- randomly-generated expressions there's a good chance that you'll
-- get division by zero (-> undefined).
instance Arbitrary F17 where
    arbitrary = F17 <$> elements [0..16]
    shrink (F17 i) = F17 <$> [0 .. i - 1]
