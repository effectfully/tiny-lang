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
    } deriving (Eq, Generic)

instance Monad m => Serial m Unique where
    series = Unique . getNonNegative <$> series

type SupplyT = StateT Unique

freshUnique :: Monad m => SupplyT m Unique
freshUnique = do
    Unique i <- get
    put . Unique $ succ i
    return $ Unique i

data Var = Var
    { _varUniq :: Unique
    , _varName :: String
    } deriving (Eq, Generic)
                  
-- TODO: use 'Pretty' and derive 'Show' as is appropriate.
instance Show Unique where
    show (Unique int) = show int

instance Show Var where
    show (Var uniq name) = name ++ "_" ++ show uniq

instance Monad m => Serial m Var where
    series = flip Var "x" <$> series

freshVar :: Monad m => String -> SupplyT m Var
freshVar name = flip Var name <$> freshUnique
