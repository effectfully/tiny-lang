module TinyLang.Field.UniConst
    ( Uni(..)
    , UniConst(..)
    , SomeUniConst
    , SomeUni
    , KnownUni
    , knownUni
    , withGeqUni
    , withGeqUniM
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
data UniConst f a = UniConst
    { _uniConstUni :: Uni f a
    , _uniConstVal :: a
    }

-- Needed for the sake of symmetry with 'UniConst'.
data UniVar f a = UniVar
    { _uniVarUni :: Uni f a
    , _uniVarVar :: Var
    }

-- -- TODO: We can can unify the two above by the following data type. Should we do that?
-- data Inhabits f a b = Inhabits
--     { _inhabitsUni :: Uni f a
--     , _inhabitsVal :: b
--     }

type SomeUniConst f = Some (UniConst f)
type SomeUni f = Some (Uni f)


-- instances

deriving instance Show (Uni f a)
deriving instance Eq   (Uni f a)
deriving instance Show (UniVar f a)

mapUniConst :: (a -> a) -> UniConst f a -> UniConst f a
mapUniConst f (UniConst uni x) = UniConst uni $ f x

zipUniConst :: (a -> a -> a) -> UniConst f a -> UniConst f a -> UniConst f a
zipUniConst f (UniConst uni x) (UniConst _ y) = UniConst uni $ f x y

mapUniConstF :: Functor g => (a -> g a) -> UniConst f a -> g (UniConst f a)
mapUniConstF f (UniConst uni x) = UniConst uni <$> f x

zipUniConstF :: Functor g => (a -> a -> g a) -> UniConst f a -> UniConst f a -> g (UniConst f a)
zipUniConstF f (UniConst uni x) (UniConst _ y) = UniConst uni <$> f x y

instance (Field f, af ~ AField f) => Field (UniConst f af) where
    zer = UniConst Field zer
    neg = mapUniConst  neg
    add = zipUniConst  add
    sub = zipUniConst  sub
    one = UniConst Field one
    inv = mapUniConstF inv
    mul = zipUniConst  mul
    div = zipUniConstF div

deriving via AField (UniConst f af) instance (Field f, af ~ AField f) => Num        (UniConst f af)
deriving via AField (UniConst f af) instance (Field f, af ~ AField f) => Fractional (UniConst f af)

instance TextField f => Show (UniConst f a) where
    show (UniConst Bool   b) = "(UniConst Bool " ++ show b ++ ")"
    show (UniConst Field  i) = showField i
    show (UniConst Vector v) = "(UniConst Vector " ++ show v ++ ")"

deriving instance TextField f => Show (Some (UniConst f))
deriving instance TextField f => Show (Some (Uni f))

withGeqUni :: Uni f a1 -> Uni f a2 -> b -> (a1 ~ a2 => b) -> b
withGeqUni Bool   Bool   _ y = y
withGeqUni Field  Field  _ y = y
withGeqUni Vector Vector _ y = y
withGeqUni Bool   _      z _ = z
withGeqUni Field  _      z _ = z
withGeqUni Vector _      z _ = z

withGeqUniM :: MonadError e m => Uni f a1 -> Uni f a2 -> e -> (a1 ~ a2 => b) -> m b
withGeqUniM Bool   Bool   _ y = pure y
withGeqUniM Field  Field  _ y = pure y
withGeqUniM Vector Vector _ y = pure y
withGeqUniM Bool   _      e _ = throwError e
withGeqUniM Field  _      e _ = throwError e
withGeqUniM Vector _      e _ = throwError e

-- This doesn't type check:
--
-- > UniConst _ x1 == UniConst _ x2 = x1 == x2
--
-- because it requires the type of @x1@ and @x2@ to have an @Eq@ instance.
-- We could provide a similar to 'withGeqUni' combinator that can handle this situation,
-- but then it's easier to just pattern match on universes.
instance Eq f => Eq (UniConst f a) where
    UniConst Bool   bool1 == UniConst Bool   bool2 = bool1 == bool2
    UniConst Field  el1   == UniConst Field  el2   = el1 == el2
    UniConst Vector vec1  == UniConst Vector vec2  = vec1 == vec2
