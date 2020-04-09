module TinyLang.Field.NamedType where

import           Data.Field.F17
import qualified TinyLang.Field.Jubjub as JJ

newtype NamedType f = NamedType String

f17 :: NamedType F17
f17 = NamedType "F17"

rational :: NamedType Rational
rational = NamedType "Rational"

jubjubF :: NamedType JJ.F
jubjubF = NamedType "JubjubF"
