module Identity where

newtype Identity a = Identity a deriving (Show, Eq, Ord)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) = fmap f

