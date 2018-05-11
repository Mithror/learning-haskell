module EitherMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second  b) = Second (f b)

instance Applicative (Sum a) where
    pure = Second
    (<*>) _ (First a) = First a
    (<*>) (First a) _ = First a
    (<*>) (Second f) (Second b) = Second (f b)

instance Monad (Sum a) where
    return = pure
    (First a) >>= _  = First a
    (Second b) >>= k = k b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [First a, Second b]

instance (Eq a, Eq b) => EqProp (Sum a b) where
    (=-=) = eq

type SumType = Sum String (Int,Int,Int)

main :: IO ()
main = do
    quickBatch (monad (undefined :: SumType))

-- ********************************
-- Associativity
-- (m >>= f) >>= g
-- join (fmap g (join (fmap f m)))

-- We can't just do
-- m >>= (f >>= g) because f is not of type (Monoid m => m b)
-- We want to pass an m to an h
-- m >>= h
-- where h is to be determined, we know it is of form (Monoid m => a -> m b)
-- and it should be based on f >>= g, this doesn't work, but we could apply
-- f to an `a` and provide that to the:
-- h x = f x >>= g
-- This is can be done via anonymous function:
-- m >>= (\x -> f x >>= g)
-- join (fmap (\x -> join (fmap g (f x))) m)