module TinyLang.Field.F17
    ( F17 (unF17)
    , toF17
    ) where

import           TinyLang.Field.Core

import           Test.QuickCheck

{-| A quick implementation of the field of seventeen elements for
  testing.  We just wrap integers in a newtype and perform operations
  modulo 17, with inverses hard-coded in a table. |-}

newtype F17 = F17 { unF17 :: Int }
    deriving (Eq)

-- Always use mod, not rem! Rem gives the wrong result for negative values.
toF17 :: Int -> F17
toF17 i = F17 $ i `mod` 17

instance (Show) F17 where
    show (F17 n) = show n

instance Field F17 where
    zer = F17 0
    one = F17 1
    add (F17 m) (F17 n) = toF17 $ m + n
    sub (F17 m) (F17 n) = toF17 $ m - n
    mul (F17 m) (F17 n) = toF17 $ m * n
    inv (F17 n) = F17 $ case n of
        0  -> error "F17: divide by zero"
        1  -> 1
        2  -> 9
        3  -> 6
        4  -> 13
        5  -> 7
        6  -> 3
        7  -> 5
        8  -> 15
        9  -> 2
        10 -> 12
        11 -> 14
        12 -> 10
        13 -> 4
        14 -> 11
        15 -> 8
        16 -> 16
        _  -> error $ "F17 is not in the [0, 16] range: " ++ show n

-- | Choose a random element uniformly. If you use this in
-- randomly-generated expressions there's a good chance that you'll
-- get division by zero (-> undefined).
instance Arbitrary F17 where
    arbitrary = F17 <$> elements [0..16]
