{-# LANGUAGE FlexibleContexts #-}
module Chapter where

import Data.Monoid ((<>))

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

-- Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
    fmap f (Identity a) = Identity $ f a
instance Foldable Identity where
    foldMap f (Identity a) = f a
instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary
instance (Eq a) => EqProp (Identity a) where (=-=) = eq

-- Constant
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)
instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a
instance Foldable (Constant a) where
    foldMap _ _ = mempty
instance Traversable (Constant a) where
    traverse _ (Constant a) = Constant <$> pure a
instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary
instance Eq a => EqProp (Constant a b) where (=-=) = eq

-- Maybe
data Optional a = Nada | Yep a deriving (Eq, Show)
instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep $ f a
instance Foldable Optional where
    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a
instance Traversable Optional where
    traverse _ Nada = pure Nada
    traverse f (Yep a) = Yep <$> f a
instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = do
        a <- arbitrary
        frequency [(1, return Nada),(3, return $ Yep a)]
instance Eq a => EqProp (Optional a) where (=-=) = eq

-- List
data List a = Nil | Cons a (List a) deriving (Eq, Show)
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a l) = Cons (f a) (fmap f l)
instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (Cons a l) = f a <> foldMap f l
instance Traversable List where
    traverse _ Nil = pure Nil
    traverse f (Cons a l) =  Cons <$> f a <*> traverse f l
toList :: [a] -> List a
toList [] = Nil
toList (a:l) = Cons a (toList l)
instance Arbitrary a => Arbitrary (List a) where
    arbitrary = toList <$> listOf arbitrary
instance Eq a => EqProp (List a) where (=-=) = eq

-- Three
data Three a b c  = Three a b c deriving (Eq, Show)
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b $ f c
instance Foldable (Three a b) where
    foldMap f (Three _ _ c) = f c
instance Traversable (Three a b) where
    traverse f (Three a b c) = Three a b <$> f c
instance (Arbitrary a, Arbitrary b, Arbitrary c) => 
         Arbitrary (Three a b c) where
            arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

-- Pair
data Pair a b = Pair a b deriving (Eq, Show)
instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a $ f b
instance Foldable (Pair a) where
    foldMap f (Pair _ b) = f b
instance Traversable (Pair a) where
    traverse f (Pair a b) = Pair a <$> f b
instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = Pair <$> arbitrary <*> arbitrary
instance (Eq a, Eq b) => EqProp (Pair a b) where (=-=) = eq

-- Big
data Big a b = Big a b b deriving (Eq, Show)
instance Functor (Big a) where
    fmap f (Big a b b') = Big a (f b) (f b')
instance Foldable (Big a) where
    foldMap f (Big _ b b') = f b <> f b'
instance Traversable (Big a) where
    traverse f (Big a b b') = Big a <$> f b <*> f b'
instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary
instance (Eq a, Eq b) => EqProp (Big a b) where (=-=) = eq

-- Bigger
data Bigger a b = Bigger a b b b deriving (Eq, Show)
instance Functor (Bigger a) where
    fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')
instance Foldable (Bigger a) where
    foldMap f (Bigger _ b b' b'') = f b <> f b' <> f b''
instance Traversable (Bigger a) where
    traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''
instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
    arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance (Eq a, Eq b) => EqProp (Bigger a b) where (=-=) = eq

-- S
data S n a = S (n a) a deriving (Eq, Show)
instance Functor n => Functor (S n) where
    fmap f (S n a) = S (fmap f n) (f a)
instance Foldable n => Foldable (S n) where
    foldMap f (S n a) = foldMap f n <> f a 
instance Traversable n => Traversable (S n) where
    traverse f (S n a) = S <$> traverse f n <*> f a
instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary
-- The EqProp instance from the book fails. No idea why, the logic seems
-- sound?
-- instance (Applicative n, Testable (n Property), EqProp a) => 
--          EqProp (S n a) where
--     (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)
instance (Eq (n a), Eq a) => EqProp (S n a) where (=-=) = eq

-- Tree
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)
instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node t a t') = Node (fmap f t) (f a) (fmap f t')
instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node t a t') = foldMap f t <> f a <> foldMap f t'
    -- foldr for extra credits!
    foldr _ z Empty = z
    foldr f z (Leaf a) = f a z
    foldr f z (Node t a t') = f a (foldr f (foldr f z t') t)
instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node t a t') = 
        Node <$> (traverse f t) <*> f a <*> traverse f t'
-- probably not the best distribution for a tree.
-- Should probably have it have a certain depth similar to []
instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = do
        a  <- arbitrary
        t  <- arbitrary
        t' <- arbitrary
        frequency [(1, return Empty),
                   (3, return $ Leaf a),
                   (3, return $ Node t a t')]
instance (Eq a) => EqProp (Tree a) where (=-=) = eq

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b 
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

main :: IO ()
main = do
    quickBatch (functor (undefined :: Identity (Int,Int,Int)))
    quickBatch (traversable (undefined :: Identity (Int,Int,[Int])))
    quickBatch (functor (undefined :: Constant String (Int,Int,Int)))
    quickBatch (traversable (undefined :: Constant String (Int,Int,[Int])))
    quickBatch (functor (undefined :: Optional (Int,Int,Int)))
    quickBatch (traversable (undefined :: Optional (Int,Int,[Int])))
    quickBatch (functor (undefined :: List (Int,Int,Int)))
    quickBatch (traversable (undefined :: List (Int,Int,[Int])))
    quickBatch (functor (undefined :: Three String Int (Int,Int,Int)))
    quickBatch (traversable (undefined :: Three String Int (Int,Int,[Int])))
    quickBatch (functor (undefined :: Pair String (Int,Int,Int)))
    quickBatch (traversable (undefined :: Pair String (Int,Int,[Int])))
    quickBatch (functor (undefined :: Big String (Int,Int,Int)))
    quickBatch (traversable (undefined :: Big String (Int,Int,[Int])))
    quickBatch (functor (undefined :: Bigger String (Int,Int,Int)))
    quickBatch (traversable (undefined :: Bigger String (Int,Int,[Int])))
    quickBatch (functor (undefined :: S [] (Int,Int,Int)))
    quickBatch (traversable (undefined :: S [] (Int,Int,[Int])))
    quickBatch (functor (undefined :: Tree (Int,Int,Int)))
    quickBatch (traversable (undefined :: Tree (Int,Int,[Int])))