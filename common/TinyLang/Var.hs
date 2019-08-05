{-# LANGUAGE UndecidableInstances #-}

module TinyLang.Var
    ( Unique (..)
    , SupplyT (..)
    , Supply
    , MonadSupply (..)
    , runSupplyT
    , runSupply
    , supplyFrom
    , freshUnique
    , Var (..)
    , freshVar
    ) where

import           TinyLang.Prelude

import           Control.Monad.Morph

-- TODO: Use a library.
newtype Unique = Unique
    { unUnique :: Int
    } deriving (Eq, Ord, Generic)

instance Monad m => Serial m Unique where
    series = Unique . getNonNegative <$> series

newtype SupplyT m a = SupplyT
    { unSupplyT :: StateT Unique m a
    } deriving newtype
        ( Functor, Applicative, Monad
        , MonadTrans, MonadReader r, MonadError e
        , MFunctor
        )

instance MonadState s m => MonadState s (SupplyT m) where
    get = lift get
    put = lift . put
    state = lift . state

type Supply = SupplyT Identity

class Monad m => MonadSupply m where
    liftSupply :: Supply a -> m a
    default liftSupply :: (m ~ t n, MonadTrans t, MonadSupply n) => Supply a -> m a
    liftSupply = lift . liftSupply

instance Monad m => MonadSupply (SupplyT m) where
    liftSupply (SupplyT a) = SupplyT $ hoist generalize a

instance MonadSupply m => MonadSupply (ExceptT e m)
instance MonadSupply m => MonadSupply (MaybeT m)
instance MonadSupply m => MonadSupply (ReaderT r m)
instance MonadSupply m => MonadSupply (StateT s m)

runSupplyT :: Monad m => SupplyT m a -> m a
runSupplyT (SupplyT a) = evalStateT a $ Unique 0

runSupply :: Supply a -> a
runSupply = runIdentity . runSupplyT

supplyFrom :: MonadSupply m => Unique -> m ()
supplyFrom = liftSupply . SupplyT . put

freshUnique :: MonadSupply m => m Unique
freshUnique = liftSupply . SupplyT $ do
    Unique i <- get
    put . Unique $ succ i
    return $ Unique i

data Var = Var
    { _varUniq :: Unique
    , _varName :: String
    } deriving (Generic)

instance Show Unique where
    show (Unique int) = show int

instance Show Var where
    show (Var uniq name) = name ++ "_" ++ show uniq

-- This is deeply inefficient, but we do not really care about efficiency, unlike correctness.
-- We should consider having a representation like
--
-- > newtype UniqueName (n :: Nat) = UniqueName String
-- > data Var = forall n. KnownNat n => Var (UniqueName n)
--
-- although that doesn't look super efficient either.
instance Eq Var where
    Var i s == Var j t
        | i == j && s == t = True
        | i == j           = error $ concat
            [ "panic: names of variables with the same unique "
            , show $ unUnique i
            , "differ: "
            , s
            , " /= "
            , t
            ]
        | otherwise        = False

instance Ord Var where
    Var i _ `compare` Var j _ = i `compare` j

instance Monad m => Serial m Var where
    series = flip Var "x" <$> series

freshVar :: MonadSupply m => String -> m Var
freshVar name = flip Var name <$> freshUnique
