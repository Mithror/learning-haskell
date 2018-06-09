module ReaderT where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

data ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
    fmap f (ReaderT rma) = ReaderT $ \r -> fmap f (rma r)

instance Applicative m => Applicative (ReaderT r m) where
    pure = ReaderT . const . pure

    (ReaderT rmf) <*> (ReaderT rma) = ReaderT $ \r ->
        (rmf r) <*> (rma r)

instance Monad m => Monad (ReaderT r m) where
    return = pure

    (ReaderT rma) >>= k = ReaderT $ \r -> do
        a <- rma r
        runReaderT (k a) r

instance MonadTrans (ReaderT r) where
    lift = ReaderT . const

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO
