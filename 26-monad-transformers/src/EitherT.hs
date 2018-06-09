module EitherT where

import Control.Monad.Trans.Class

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }


instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

instance Applicative m => Applicative (EitherT e m) where
    pure a = EitherT $ pure . pure $ a
    (EitherT mf) <*> (EitherT ma) = EitherT $ (fmap (<*>) mf) <*> ma

instance Monad m => Monad (EitherT e m) where
    return = pure
    (EitherT ma) >>= k = EitherT $ do
        a <- ma
        case a of
            Right a' -> runEitherT $ k a'
            Left e -> return $ Left e

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ma) = EitherT $ fmap swapEither ma

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT ma) = do
    a <- ma
    case a of
        Left e -> f e
        Right a' -> g a'

instance MonadTrans (EitherT e) where
    lift m = EitherT $ fmap Right m