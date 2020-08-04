module TinyLang.Field.Uni
    ( Uni(..)
    , UniConst(..)
    , UniVar(..)
    , SomeUniConst
    , SomeUniVar
    , SomeUni
    , KnownUni
    , knownUni
    , withGeqUni
    , withGeqUniM
    , mkSomeUniVar
    ) where

import           Data.Field
import           Prelude                    hiding (div)
import           TinyLang.Field.Existential
import           TinyLang.Prelude
import           TinyLang.Var

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

-- NOTE:  Custom Show instance below
-- Needed for the sake of deriving.
data UniConst f a = UniConst
    { _uniConstUni :: Uni f a
    , _uniConstVal :: a
    } deriving (Ord)

-- Needed for the sake of symmetry with 'UniConst'.
data UniVar f a = UniVar
    { _uniVarUni :: Uni f a
    , _uniVarVar :: Var
    } deriving (Eq, Ord, Show)

-- -- TODO: We can can unify the two above by the following data type. Should we do that?
-- data Inhabits f a b = Inhabits
--     { _inhabitsUni :: Uni f a
--     , _inhabitsVal :: b
--     }

type SomeUniConst f = Some (UniConst f)
type SomeUniVar f   = Some (UniVar f)
type SomeUni f      = Some (Uni f)


-- instances

deriving instance Show (Uni f a)
deriving instance Eq   (Uni f a)
-- NOTE:  This instance fixes the uni (similarly to Eq)
deriving instance Ord  (Uni f a)

deriving instance Show (SomeUniVar f)

instance Eq (SomeUni f) where
    Some u1 == Some u2 = withGeqUni u1 u2 False True

instance Ord (SomeUni f) where
    Some Bool   `compare` Some Bool   = EQ
    Some Bool   `compare` Some Field  = LT
    Some Bool   `compare` Some Vector = LT
    Some Field  `compare` Some Bool   = GT
    Some Field  `compare` Some Field  = EQ
    Some Field  `compare` Some Vector = LT
    Some Vector `compare` Some Bool   = GT
    Some Vector `compare` Some Field  = GT
    Some Vector `compare` Some Vector = EQ

-- NOTE:  We require that the type of variable matches
instance Eq (SomeUniVar f) where
    Some (UniVar u1 v1) == Some (UniVar u2 v2) = withGeqUni u1 u2 False (v1 == v2)

-- NOTE:  We compare unis, if they are the same, we compare variables
instance Ord (SomeUniVar f) where
    Some (UniVar u1 v1) `compare` Some (UniVar u2 v2) =
        withGeqUni u1 u2 (Some u1 `compare` Some u2) (v1 `compare` v2)

-- NOTE:  Technically, we could use an automatically derived instance and push
-- the @Eq a@ to `Typed.Core.Expr`
-- NOTE: UniConst _ x1 == UniConst x2 will not work, as we need an Eq a
-- instance.
instance Eq f => Eq (UniConst f a) where
    UniConst Bool   b1 == UniConst Bool   b2 = b1 == b2
    UniConst Field  f1 == UniConst Field  f2 = f1 == f2
    UniConst Vector v1 == UniConst Vector v2 = v1 == v2

-- NOTE: We explicitly match on all universes in False case to ensure that the
-- compiler will report an error when a new Uni is added.
instance Eq f => Eq (SomeUniConst f) where
    Some (UniConst Bool b1)   == Some (UniConst Bool b2)   = b1 == b2
    Some (UniConst Field f1)  == Some (UniConst Field f2)  = f1 == f2
    Some (UniConst Vector v1) == Some (UniConst Vector v2) = v1 == v2
    Some (UniConst Bool _)    == _                         = False
    Some (UniConst Field _)   == _                         = False
    Some (UniConst Vector _)  == _                         = False

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

mkSomeUniVar :: forall f. SomeUni f -> Var -> SomeUniVar f
mkSomeUniVar (Some uni) var = Some $ UniVar uni var
