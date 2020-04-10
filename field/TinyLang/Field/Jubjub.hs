-- | This module is intended to be imported qualified as @JJ@.

module TinyLang.Field.Jubjub
    ( F
    , u
    , r
    , d
    ) where

import           Data.Field
import qualified Data.Field.Galois as GF

type F = GF.Prime 52435875175126190479447740508185965837690552500527637822603658699938581184513

u :: Integer
u = -0xd201000000010000

-- This is the long number from the above. Would be nice to statically test they're equal somehow.
-- | The characteristic of the 'F' field.
-- >>> toInteger (GF.char (undefined :: F)) == r
-- True
r :: Integer
r = let u2 = u*u in u2*u2 - u2 + 1

d :: AField F
d = fromInteger $ toInteger (-10240/10241 :: F) `mod` r
