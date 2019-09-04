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
import           Test.SmallCheck.Series    as Export
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

infixr 9 .*

(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.*) = (.) . (.)

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

visitExtract :: (Functor t, Functor f) => (t a -> a) -> (a -> f b) -> t a -> f (t b)
visitExtract ext f a = (<$ a) <$> f (ext a)
