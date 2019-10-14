module Data.NamedType where

import           Data.Field.F17
import           Data.Field.F4913

newtype NamedType f = NamedType String

f17 :: NamedType F17
f17 = NamedType "F17"

f4913 :: NamedType F4913
f4913 = NamedType "F4913"

rational :: NamedType Rational
rational = NamedType "Rational"
