{-# LANGUAGE InstanceSigs #-}
module IdentityT where

newtype IdentityT f a = IdentityT { runIdentityT :: f a }

instance Functor f => Functor (IdentityT f) where
    fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance Applicative f => Applicative (IdentityT f) where
    pure a = IdentityT $ pure a

    (IdentityT ff) <*> (IdentityT fa) =
        IdentityT $ ff <*> fa

instance Monad m => Monad (IdentityT m) where
    return = pure

    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (IdentityT ma) >>= f =
        IdentityT $ ma >>= (runIdentityT . f)
