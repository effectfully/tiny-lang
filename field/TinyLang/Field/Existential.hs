module TinyLang.Field.Existential
    ( Some (..)
    , SomeOf (..)
    , Forget (..)
    , traverseSomeOf
    ) where

data Some (f :: k -> *) = forall x. Some (f x)
data SomeOf uni (f :: k -> *) = forall x. SomeOf (uni x) (f x)

class Forget some where
    forget :: (forall x. f x -> r) -> some f -> r

instance Forget Some where
    forget f (Some a) = f a

instance Forget (SomeOf uni) where
    forget f (SomeOf _ a) = f a

traverseSomeOf :: Functor m => (forall a. f a -> m (f a)) -> SomeOf uni f -> m (SomeOf uni f)
traverseSomeOf f (SomeOf uni a) = SomeOf uni <$> f a


