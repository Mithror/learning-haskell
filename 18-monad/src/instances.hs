module Instances where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Control.Applicative (liftA2)

-- 1
data Nope a = NopeDotJpg deriving (Eq, Show)
instance Functor Nope where
    fmap _ _ = NopeDotJpg
instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg
instance Monad Nope where
    return _ = NopeDotJpg
    (>>=) _ _ = NopeDotJpg
instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg
instance EqProp (Nope a) where
    (=-=) = eq
type NopeType = Nope (Int,Int,Int)

-- 2
data PhhhbbtttEither b a = Left' a | Right' b deriving (Eq, Show)
instance Functor (PhhhbbtttEither b) where
    fmap _ (Right' b) = Right' b
    fmap f (Left' a)  = Left' (f a)
instance Monoid b => Applicative (PhhhbbtttEither b) where
    pure = Left'
    (Right' b) <*> (Right' b') = Right' $ b `mappend` b'
    (Right' b) <*> _ = Right' b
    _ <*> (Right' b) = Right' b
    (Left' f) <*> (Left' a) = Left' (f a)
instance Monoid b => Monad (PhhhbbtttEither b) where
    return = Left'
    (Right' b) >>= _ = Right' b
    (Left' a) >>= k  = k a
instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Left' a, return $ Right' b]
instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
    (=-=) = eq
type PhhhbbtttEitherType = PhhhbbtttEither String (Int,Int,Int)

-- 3
newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
    fmap f (Identity a) = Identity $ f a
instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity $ f a
instance Monad Identity where
    return = Identity
    (Identity a) >>= k = k a
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = arbitrary >>= return . Identity
instance Eq a => EqProp (Identity a) where
    (=-=) = eq
type IdentityType = Identity (Int,Int,Int)

-- 4
data List a = Nil | Cons a (List a) deriving (Eq, Show)
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a l) = Cons (f a) (fmap f l)
instance Monoid (List a) where
    mempty = Nil
    mappend l Nil = l
    mappend Nil l = l
    mappend (Cons a l) l' = Cons a (mappend l l')
instance Applicative List where
    pure = flip Cons Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f fl) <*> as = (fmap f as) `mappend` (fl <*> as)
instance Monad List where
    return = flip Cons Nil
    Nil >>= _ = Nil
    (Cons a l) >>= k = (k a) `mappend` (l >>= k)

-- The following is needed to get an Arbitrary for the List. I got the idea
-- from looking at how it is done for []. It's not very efficient though.
replicateM' :: Applicative m => Int -> m a -> m (List a)
replicateM' cnt0 f = loop cnt0
    where loop cnt
                | cnt <= 0 = pure Nil
                | otherwise = liftA2 Cons f (loop (cnt - 1))

listOf' :: Gen a -> Gen (List a)
listOf' gen = sized $ \n ->
    do k <- choose (0,n)
       replicateM' k gen

-- toList :: [a] -> List a
-- toList [] = Nil
-- toList (x:xs) = Cons x $ toList xs
instance Arbitrary a => Arbitrary (List a) where
    arbitrary = listOf' arbitrary
    -- arbitrary = fmap toList (listOf arbitrary)
    -- arbitrary = do
    --     a <- arbitrary
    --     l <- arbitrary
    --     frequency [(1, return Nil), (3, return $ Cons a l)]
instance Eq a => EqProp (List a) where
    (=-=) = eq
type ListType = List (Int,Int,Int)

main :: IO ()
main = do
    quickBatch (functor  (undefined :: NopeType))
    quickBatch (applicative (undefined :: NopeType))
    quickBatch (monad (undefined :: NopeType))

    quickBatch (functor (undefined :: PhhhbbtttEitherType))
    quickBatch (applicative (undefined :: PhhhbbtttEitherType))
    quickBatch (monad (undefined :: PhhhbbtttEitherType))

    quickBatch (functor (undefined :: IdentityType))
    quickBatch (applicative (undefined :: IdentityType))
    quickBatch (monad (undefined :: IdentityType))

    -- [a]
    quickBatch (functor (undefined :: [(Int,Int,Int)]))
    quickBatch (monoid (undefined :: [(Int,Int,Int)]))
    quickBatch (applicative (undefined :: [(Int,Int,Int)]))
    quickBatch (monad (undefined :: [(Int,Int,Int)]))

    quickBatch (functor (undefined :: ListType))
    quickBatch (monoid (undefined :: ListType))
    quickBatch (applicative (undefined :: ListType))
    quickBatch (monad (undefined :: ListType))
