{-# LANGUAGE UndecidableInstances #-}

module TinyLang.Var
    ( Unique (..)
    , SupplyT (..)
    , Supply
    , MonadSupply (..)
    , freshUnique
    , Var (..)
    , freshVar
    ) where

import           TinyLang.Prelude

import           Control.Monad.Morph

-- TODO: Use a library.
newtype Unique = Unique
    { unUnique :: Int
    } deriving (Eq, Generic)

instance Monad m => Serial m Unique where
    series = Unique . getNonNegative <$> series

newtype SupplyT m a = SupplyT
    { unSupplyT :: StateT Unique m a
    } deriving newtype
        ( Functor, Applicative, Monad
        , MonadTrans, MonadError e, MonadReader r
        , MFunctor
        )

instance MonadState s m => MonadState s (SupplyT m) where
    get = lift get
    put = lift . put
    state = lift . state

type Supply = SupplyT Identity

class Monad m => MonadSupply m where
    liftSupply :: Supply a -> m a

instance MonadSupply m => MonadSupply (SupplyT m) where
    liftSupply (SupplyT a) = SupplyT $ hoist generalize a

freshUnique :: MonadSupply m => m Unique
freshUnique = liftSupply . SupplyT $ do
    Unique i <- get
    put . Unique $ succ i
    return $ Unique i

data Var = Var
    { _varUniq :: Unique
    , _varName :: String
    } deriving (Eq, Generic)

instance Show Unique where
    show (Unique int) = show int

instance Show Var where
    show (Var uniq name) = name ++ "_" ++ show uniq

instance Monad m => Serial m Var where
    series = flip Var "x" <$> series

freshVar :: MonadSupply m => String -> m Var
freshVar name = flip Var name <$> freshUnique
