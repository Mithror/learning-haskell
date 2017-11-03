module Validation' where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation' e a = Failure' e | Success' a deriving (Eq, Show)

instance Functor (Validation' e) where
    fmap _ (Failure' e) = Failure' e
    fmap f (Success' s) = Success' $ f s

instance Monoid e => Applicative (Validation' e) where
    pure = Success'
    (Failure' e) <*> (Failure' e') = Failure' $ e `mappend` e'
    (Failure' e) <*> _ = Failure' e
    _ <*> (Failure' e) = Failure' e
    (Success' f) <*> s = fmap f s

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Failure' a, Success' b]

instance (Eq a, Eq b) => EqProp (Validation' a b) where
    (=-=) (Failure' _) (Success' _) = property False
    (=-=) (Success' _) (Failure' _) = property False
    (=-=) a b = eq a b

test :: Validation' String (Int, Int, Int)
test = undefined

main :: IO ()
main = do
    quickBatch (applicative test)