{-# OPTIONS_GHC -fno-warn-orphans #-}

module TinyLang.Prelude
    ( module Export
    , module TinyLang.Prelude
    ) where

-- base
------------------------------
import           Control.Applicative       as Export
import           Control.Exception         as Export
import           Control.Monad             as Export
import           Control.Monad.IO.Class    as Export
import           Data.Bifunctor            as Export
import           Data.Bool                 as Export
import           Data.Char                 as Export
import           Data.Coerce               as Export
import           Data.Complex              as Export
import           Data.Either               as Export
import           Data.Foldable             as Export
import           Data.Function             as Export
import           Data.Functor              as Export
import           Data.Functor.Compose      as Export
import           Data.Functor.Identity     as Export
import           Data.List                 as Export
import           Data.Maybe                as Export
import           Data.Monoid               as Export hiding (First (..),
                                                      Last (..))
import           Data.Ord                  as Export
import           Data.Proxy                as Export
import           Data.Ratio                as Export
import           Data.Semigroup            as Export
import           Data.String               as Export
import           Data.Traversable          as Export
import           Data.Tuple                as Export
import           Data.Void                 as Export
import           Debug.Trace               as Export
import           GHC.Exts                  as Export (groupWith, sortWith)
import           GHC.Generics              as Export (Generic, Generic1)
import           GHC.Stack                 as Export
import           Numeric                   as Export
import           System.IO                 as Export
import           Text.Read                 as Export (Read (..), readEither,
                                                      readMaybe)

-- transformers
------------------------------
import           Control.Monad.Trans.Maybe as Export

-- mtl
------------------------------
import           Control.Monad.Except      as Export
import           Control.Monad.Reader      as Export
import           Control.Monad.State       as Export


-- containers
------------------------------
import           Data.IntMap.Strict        as Export (IntMap)
import           Data.IntSet               as Export (IntSet)
import           Data.Map.Strict           as Export (Map)
import           Data.Set                  as Export (Set)

-- hashable
------------------------------
import           Data.Hashable             as Export

-- unordered-containers
------------------------------
import           Data.HashMap.Strict       as Export (HashMap)
import           Data.HashSet              as Export (HashSet)

import qualified Data.IntMap.Strict        as IntMap

import           Test.QuickCheck.Property

-- vector
------------------------------

import           Data.Vector               as Export (Vector)

infixr 9 .*
infixr 2 ?

newtype PairT b f a = PairT
    { unPairT :: f (b, a)
    }

instance Functor f => Functor (PairT b f) where
    fmap f (PairT p) = PairT $ fmap (fmap f) p

instance Hashable a => Hashable (IntMap a) where
    hashWithSalt salt = hashWithSalt salt . IntMap.toList

-- Feels so weird not to have it by default.
instance Testable (Either String ()) where
    property = property . \case
        Left err -> failed { reason = err }
        Right () -> succeeded

(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.*) = (.) . (.)
{-# INLINABLE (.*) #-}

(?) :: Alternative f => Bool -> a -> f a
(?) b x = x <$ guard b
{-# INLINABLE (?) #-}

visitExtract :: (Functor t, Functor f) => (t a -> a) -> (a -> f b) -> t a -> f (t b)
visitExtract ext f a = (<$ a) <$> f (ext a)
{-# INLINABLE visitExtract #-}

firstF :: Functor f => (a -> f c) -> (a, b) -> f (c, b)
firstF f (x, y) = flip (,) y <$> f x
{-# INLINABLE firstF #-}

foldlM' :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM' f z xs = foldr step return xs z where
  step x r a = f a x >>= (r $!)
{-# INLINABLE foldlM' #-}

foldMapA :: (Foldable t, Applicative f, Monoid b) => (a -> f b) -> t a -> f b
foldMapA f = foldr (\x r -> mappend <$> f x <*> r) (pure mempty)
{-# INLINABLE foldMapA #-}

-- | Fold a monadic function over a 'Foldable'. The monadic version of 'foldMap'.
foldMapM :: (Foldable f, Monad m, Monoid b) => (a -> m b) -> f a -> m b
foldMapM f xs = foldr step return xs mempty where
    step x r z = f x >>= \y -> r $! z `mappend` y
{-# INLINABLE foldMapM #-}

asumMap :: (Foldable t, Alternative f) => (a -> f b) -> t a -> f b
asumMap f = foldr ((<|>) . f) empty
{-# INLINABLE asumMap #-}

mapMaybeA :: Applicative f => (a -> f (Maybe b)) -> [a] -> f [b]
mapMaybeA f = foldr (\x r -> maybe id (:) <$> f x <*> r) (pure empty)
{-# INLINABLE mapMaybeA #-}

fromOption :: (Foldable f, Applicative g) => g a -> f a -> g a
fromOption = foldr (const . pure)
{-# INLINABLE fromOption #-}

-- | This function generalizes 'eitherToMaybe', 'eitherToList',
-- 'listToMaybe' and other such functions.
reoption :: (Foldable f, Alternative g) => f a -> g a
reoption = fromOption empty
{-# INLINABLE reoption #-}

forBind :: (Monad m, Traversable m, Applicative f) => m a -> (a -> f (m b)) -> f (m b)
forBind a f = join <$> traverse f a
{-# INLINABLE forBind #-}
