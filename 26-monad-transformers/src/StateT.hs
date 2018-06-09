module StateT where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

data StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
    fmap f st = StateT $ \s ->
        fmap (\(a, s') -> (f a, s')) $ runStateT st s

instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \s -> pure (a, s)
    stf <*> sta = StateT $ \s -> do
        (f, s') <- runStateT stf s
        (a, s'') <- runStateT sta s'
        return (f a, s'')

instance Monad m => Monad (StateT s m) where
    return = pure
    st >>= k = StateT $ \s -> do
        (a, s') <- runStateT st s
        runStateT (k a) s'

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> fmap (flip (,) s) m

instance MonadIO m => MonadIO (StateT s m) where
    liftIO = lift . liftIO