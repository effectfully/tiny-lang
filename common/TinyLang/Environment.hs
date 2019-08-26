module TinyLang.Environment
    ( Env (..)
    , mapEnv
    , lookupUnique
    , lookupVar
    , unsafeLookupUnique
    , unsafeLookupVar
    , insertUnique
    , insertVar
    , toEnvBy
    , fromUniques
    , fromVars
    , toUniques
    ) where

import           TinyLang.Prelude
import           TinyLang.Var

import qualified Data.IntMap.Strict as IntMap

-- | A simple representation of environments as 'IntMap's mapping variables to values
newtype Env a = Env
    { unEnv :: IntMap a
    } deriving (Show, Eq, Functor, Foldable, Traversable)
      deriving newtype (Semigroup, Monoid)

mapEnv :: (IntMap a -> IntMap b) -> Env a -> Env b
mapEnv = coerce

lookupUnique :: Unique -> Env a -> Maybe a
lookupUnique (Unique ind) (Env env) = IntMap.lookup ind env

lookupVar :: Var -> Env a -> Maybe a
lookupVar = lookupUnique . _varUniq

unsafeLookupUnique :: HasCallStack => Unique -> Env a -> a
unsafeLookupUnique (Unique ind) (Env env) = fromMaybe err $ IntMap.lookup ind env where
    err = error $ "The " ++ show ind ++ " unique is not present"

unsafeLookupVar :: HasCallStack => Var -> Env a -> a
unsafeLookupVar = unsafeLookupUnique . _varUniq

insertUnique :: Unique -> a -> Env a -> Env a
insertUnique (Unique i) x (Env xs) = Env $ IntMap.insert i x xs

insertVar :: Var -> a -> Env a -> Env a
insertVar = insertUnique . _varUniq

toEnvBy :: Foldable f => (a -> Env b -> Env b) -> f a -> Env b
toEnvBy f = foldl' (\acc x -> f x acc) mempty

fromUniques :: Foldable f => f (Unique, a) -> Env a
fromUniques = toEnvBy $ uncurry insertUnique

fromVars :: Foldable f => f (Var, a) -> Env a
fromVars = toEnvBy $ uncurry insertVar

toUniques :: Env a -> [(Unique, a)]
toUniques = map (first Unique) . IntMap.toList . unEnv
