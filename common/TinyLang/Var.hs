{-# LANGUAGE UndecidableInstances #-}

module TinyLang.Var
    ( Unique (..)
    , SupplyT (..)
    , Supply
    , MonadSupply (..)
    , runSupplyT
    , runSupply
    , supplyFromAtLeast
    , freshUnique
    , freeUniqueIntMap
    , freeUniqueFoldable
    , Var (..)
    , varUniq
    , freshVar
    ) where

import           TinyLang.Prelude

import           Control.Monad.Morph
import           Control.Lens
import qualified Data.IntMap.Strict  as IntMap

-- TODO: Use a library.
newtype Unique = Unique
    { unUnique :: Int
    } deriving (Eq, Ord, Enum, Generic)

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

supplyFromAtLeast :: MonadSupply m => Unique -> m ()
supplyFromAtLeast uniq' =
    liftSupply . SupplyT . modify $ \uniq ->
        if uniq >= uniq' then uniq else uniq'

freshUnique :: MonadSupply m => m Unique
freshUnique = liftSupply . SupplyT $ do
    Unique i <- get
    put . Unique $ succ i
    return $ Unique i

freeUniqueIntMap :: IntMap a -> Unique
freeUniqueIntMap = Unique . maybe 0 (succ . fst) . IntMap.lookupMax

freeUniqueFoldable :: Foldable f => f Unique -> Unique
freeUniqueFoldable = succ . foldl' max (Unique (-1))

data Var = Var
    { _varUniq :: Unique
    , _varName :: String
    } deriving (Generic)

instance Show Unique where
    show (Unique int) = show int

instance Show Var where
    show (Var uniq name) = name ++ "_" ++ show uniq

-- NOTE: For efficiency and alpha equivalence tests we only compare the Unique
-- in the variable.
instance Eq Var where
    Var i _ == Var j _ = i == j

instance Ord Var where
    Var i _ `compare` Var j _ = i `compare` j

varUniq :: Lens' Var Unique
varUniq = lens _varUniq $ \var uniq -> var { _varUniq = uniq }

freshVar :: MonadSupply m => String -> m Var
freshVar name = flip Var name <$> freshUnique
