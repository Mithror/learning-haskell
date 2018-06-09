module MaybeT where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

data MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
    pure = MaybeT . pure . Just

    (MaybeT mf) <*> (MaybeT ma) = MaybeT $
        (fmap (<*>) mf) <*> ma

instance Monad m => Monad (MaybeT m) where
    return = pure

    (MaybeT ma) >>= k = MaybeT $ do
        a <- ma
        case a of
            Nothing -> return Nothing
            Just a' -> runMaybeT $ k a'

instance MonadTrans (MaybeT) where
    lift m = MaybeT $ fmap Just m

instance MonadIO m => MonadIO (MaybeT m) where
    liftIO = lift . liftIO