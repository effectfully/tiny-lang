module TinyLang.Field.F17
    (F17(..)) where

import           Prelude
import           Test.QuickCheck
import qualified Text.Megaparsec.Char.Lexer   as L
import           TinyLang.Field.Core
import           TinyLang.Field.ParsableField
import           TinyLang.Field.ParserUtils

{-| A quick implementation of the field of seventeen elements for
  testing.  We just wrap integers in a newtype and perform operations
  modulo 17, with inverses hard-coded in a table. |-}

newtype F17 = F17 { _unF17 :: Int }
    deriving (Eq)

instance (Show) F17 where
    show (F17 n) = show n

inv17 :: Int -> Int
inv17 n =
    case n of
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
      _  -> inv17 (n `mod` 17)

-- Always use mod, not rem! Rem gives the wrong result for negative values.
instance Field F17 where
    zer = F17 0
    one = F17 1
    add (F17 m) (F17 n) = F17 $ (m+n) `mod` 17
    sub (F17 m) (F17 n) = F17 $ (m-n) `mod` 17
    mul (F17 m) (F17 n) = F17 ((m*n) `mod` 17)
    inv (F17 n) = F17 (inv17 n)


-- | Choose a random element uniformly.  If you use this in
-- randomly-generated expressions there's a good chance that you'll
-- get division by zero (-> undefined).
instance Arbitrary F17 where
    arbitrary = F17 <$> elements [0..16]

instance ParsableField F17
    where parseFieldElement = (\n -> F17 (n `mod` 17)) <$> lexeme L.decimal




