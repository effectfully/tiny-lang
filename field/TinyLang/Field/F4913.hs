module TinyLang.Field.F4913 where

import           TinyLang.Field.Core
import           TinyLang.Field.ParsableField
import           TinyLang.Field.ParserUtils
import           TinyLang.Prelude

import           Test.QuickCheck
import qualified Text.Megaparsec.Char.Lexer   as L

{- | A crude implementation of the field of 4193 = 17^3 elements, ie
     GF(17^3).  We construct F_4193 as F_17[x]/(p), where p is the
     irreducible polynomial x^3 + x + 3.  More concretely, we define
     an equivalence relation on the polynomial ring F_17[x] by saying
     that two polynomials are equivalent if they differ by a multiple
     of x^3+x+3, and F_4193 is the quotient of the polynomial ring by
     this relation.  Every element of F_4193 then has a canonical
     representation as a polynomial a+bx+cx^2.  We add and subtract
     two such polynomials by adding or subtracting corresponding
     coefficients (in F_17, so reducing modulo 17 if we're using
     ints).  To find the product, we multiply polynomials as usual and
     then use the fact that x^3 = -x-3 to dispose of terms of degree
     greater than 2: this is hard-coded into the `mul` method.  To
     find inverses we use the fact that in this field the inverse of
     any nonzero element a is a^4911 (ie, a^(17^3-2)).

     We represent a canonical polynomial a+bx+cx^2 using the
     constructor F a b c with a,b, and c Ints.  Strictly We should use
     elements of F17, but that would complicate things for little
     gain.  The concrete syntax for F a b c is [a,b,c].
-}


data F4913 = F Int Int Int  -- F a b c ~ a+bx+cx^2
             deriving (Eq)

instance Show F4913 where
    show (F a b c) = "[" ++ show a ++ "," ++ show b ++ "," ++ show c ++ "]"

reduce :: F4913 -> F4913
reduce (F a b c) = F (a `mod` 17) (b `mod` 17) (c `mod` 17)

instance Field F4913 where
    zer = F 0 0 0
    one = F 1 0 0
    add (F a b c) (F d e f) = reduce $ F (a+d) (b+e) (c+f)
    sub (F a b c) (F d e f) = reduce $ F (a-d) (b-e) (c-f)
    mul (F a b c) (F d e f) = reduce $ F (a*d - 3*c*e - 3*b*f)
                                         (b*d + (a-c)*e +(-b -3*c)*f)
                                         (c*d + b*e + (a-c)*f)
    inv (F 0 0 0) = throw DivideByZero
    inv (F a b c) = pow (F a b c) 4911

-- Exponentiation by repeated squaring
pow :: F4913 -> Int -> F4913
pow x0 n0 =
    let  pow' x n  -- use only for 0 <= n <= 4911
             | n == 0    = one
             | n == 1    = x
             | even n    = pow' (square x) (half n)
             | odd n     = mul x (pow' (square x) (half(n-1)))
             | otherwise = error "Impossible power"
             where square x' = mul x' x'
                   half n' = Prelude.div n' 2
    in if x0==zer && n0<0
       then throw DivideByZero
       else pow' x0 (n0 `mod` 4912)
       -- ... using x^4192 = 1 for x /= 0, so x^(4192k +l) = x^l; also works for n<0 unless x=0.

instance ParsableField F4913
    where parseFieldElement = F <$> (symbol "[" *> lexeme L.decimal)
                                <*> (symbol "," *> lexeme L.decimal)
                                <*> (symbol "," *> lexeme L.decimal <* symbol "]")

instance AsInteger F4913 where
    asInteger (F a b c) =
        if b==0 && c==0 then Just $ fromIntegral a
        else Nothing

instance IsNegative F4913 where
    isNegative _ = False

arbitraryConst :: Gen Int
arbitraryConst = elements [0..16]

instance Arbitrary F4913 where
    arbitrary = F <$> arbitraryConst <*> arbitraryConst <*> arbitraryConst
    shrink (F a b c) = F <$> [0 .. a-1] <*> [0..b-1] <*> [0..c-1]
    -- Is this a sensible shrinking?  We might get failures at F 0 0 0
    -- that aren't due to the original cause.
