module TinyLang.Field.UniVal
    ( Uni(..)
    , UniVal(..)
    , SomeUniVal
    , knownUni
    , withGeqUni
    ) where

import Prelude                    hiding (div)
import Data.Field
import TinyLang.Prelude
import TinyLang.Var
import TinyLang.Field.Existential

data Uni f a where
    Bool   :: Uni f Bool
    Field  :: Uni f (AField f)
    -- TODO: Check for Haddock post lts-13.26
    -- We need this additional 'AField' wrapper in order to make 'Uni'
    -- a singleton.  That is, if we made it @Field :: Uni f f@, then
    -- with @f@ instantiated to @Bool@, both @Bool@ and @Field@ would
    -- be of the same type: @Uni Bool Bool@. Since we use @Uni@ in
    -- order to reflect types at the term level, we do want it to be a
    -- singleton.  Originally @Field@ didn't use the wrapper and we
    -- were getting annoying "pattern matching is not exhaustive"
    -- warnings. Now @a@ uniquely determines the constructor and we do
    -- not have such warnings.
    Vector :: Uni f (Vector Bool)

class KnownUni f a where
    knownUni :: Uni f a

instance KnownUni f Bool where
    knownUni = Bool

instance f ~ f' => KnownUni f (AField f') where
    knownUni = Field

instance bool ~ Bool => KnownUni f (Vector bool) where
    knownUni = Vector

-- Needed for the sake of deriving.
data UniVal f a = UniVal
    { _uniValUni :: Uni f a
    , _uniValVal :: a
    }

-- Needed for the sake of symmetry with 'UniVal'.
data UniVar f a = UniVar
    { _uniVarUni :: Uni f a
    , _uniVarVar :: Var
    }

-- -- TODO: We can can unify the two above by the following data type. Should we do that?
-- data Inhabits f a b = Inhabits
--     { _inhabitsUni :: Uni f a
--     , _inhabitsVal :: b
--     }

type SomeUniVal f = Some (UniVal f)

-- instances

deriving instance Show (Uni f a)
deriving instance Eq   (Uni f a)
deriving instance Show (UniVar f a)

mapUniVal :: (a -> a) -> UniVal f a -> UniVal f a
mapUniVal f (UniVal uni x) = UniVal uni $ f x

zipUniVal :: (a -> a -> a) -> UniVal f a -> UniVal f a -> UniVal f a
zipUniVal f (UniVal uni x) (UniVal _ y) = UniVal uni $ f x y

mapUniValF :: Functor g => (a -> g a) -> UniVal f a -> g (UniVal f a)
mapUniValF f (UniVal uni x) = UniVal uni <$> f x

zipUniValF :: Functor g => (a -> a -> g a) -> UniVal f a -> UniVal f a -> g (UniVal f a)
zipUniValF f (UniVal uni x) (UniVal _ y) = UniVal uni <$> f x y

instance (Field f, af ~ AField f) => Field (UniVal f af) where
    zer = UniVal Field zer
    neg = mapUniVal  neg
    add = zipUniVal  add
    sub = zipUniVal  sub
    one = UniVal Field one
    inv = mapUniValF inv
    mul = zipUniVal  mul
    div = zipUniValF div

deriving via AField (UniVal f af) instance (Field f, af ~ AField f) => Num        (UniVal f af)
deriving via AField (UniVal f af) instance (Field f, af ~ AField f) => Fractional (UniVal f af)

instance TextField f => Show (UniVal f a) where
    show (UniVal Bool   b) = "(UniVal Bool " ++ show b ++ ")"
    show (UniVal Field  i) = showField i
    show (UniVal Vector v) = "(UniVal Vector " ++ show v ++ ")"

deriving instance TextField f => Show (Some (UniVal f))

withGeqUni :: Uni f a1 -> Uni f a2 -> b -> (a1 ~ a2 => b) -> b
withGeqUni Bool   Bool   _ y = y
withGeqUni Field  Field  _ y = y
withGeqUni Vector Vector _ y = y
withGeqUni Bool   _      z _ = z
withGeqUni Field  _      z _ = z
withGeqUni Vector _      z _ = z

instance Eq f => Eq (UniVal f a) where
    UniVal Bool   bool1 == UniVal Bool   bool2 = bool1 == bool2
    UniVal Field  el1   == UniVal Field  el2   = el1 == el2
    UniVal Vector vec1  == UniVal Vector vec2  = vec1 == vec2

instance Eq f => Eq (UniVar f a) where
    UniVar _ v1 == UniVar _ v2 = v1 == v2
