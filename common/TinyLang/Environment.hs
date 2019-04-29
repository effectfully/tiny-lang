module TinyLang.Environment
    ( Env (..)
    , lookupUnique
    , lookupVar
    ) where

import           TinyLang.Prelude
import           TinyLang.Var

import qualified Data.IntMap.Strict as IntMap

-- | A simple representation of environments as 'IntMap's mapping variables to values
newtype Env a = Env
    { unEnv :: IntMap a
    } deriving (Show, Eq, Functor)

lookupUnique :: Unique -> Env a -> a
lookupUnique (Unique ind) (Env env) = env IntMap.! ind

lookupVar :: Var -> Env a -> a
lookupVar = lookupUnique . _varUniq
