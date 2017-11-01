module Sum where

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (Second b) = Second $ f b
    fmap _ a = a