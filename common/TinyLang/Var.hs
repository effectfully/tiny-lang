module TinyLang.Var
    ( Unique (..)
    , SupplyT
    , freshUnique
    , Var (..)
    , freshVar
    ) where

import           TinyLang.Prelude

-- TODO: Use a library.
newtype Unique = Unique
    { unUnique :: Int
    }

type SupplyT = StateT Unique

freshUnique :: Monad m => SupplyT m Unique
freshUnique = do
    Unique i <- get
    put . Unique $ succ i
    return $ Unique i

data Var = Var
    { varUniq :: Unique
    , varName :: String
    }

-- TODO: use 'Pretty' and derive 'Show' as is approriate.
instance Show Unique where
    show (Unique int) = show int

instance Show Var where
    show (Var uniq name) = name ++ "_" ++ show uniq

freshVar :: Monad m => String -> SupplyT m Var
freshVar name = flip Var name <$> freshUnique
