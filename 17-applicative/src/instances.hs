module Instances where

import Data.Monoid (Sum, Product)

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1
data Pair a = Pair a a deriving (Show, Eq)
instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')
instance Applicative Pair where
    pure x = Pair x x
    (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')
instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        a' <- arbitrary
        return $ Pair a a'
instance Eq a => EqProp (Pair a) where
    (=-=) = eq
type PairType = Pair (Int, Int, Int)

-- 2
data Two a b = Two a b deriving (Show, Eq)
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)
instance Monoid a => Applicative (Two a)  where
    pure = Two mempty
    (<*>) (Two a f) (Two a' b) = Two (mappend a a') (f b)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b
instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq
type TwoType = Two String (Int,Int,Int)

-- 3
data Three a b c = Three a b c deriving (Show, Eq)
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)
instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure x = Three mempty mempty x
    (<*>) (Three a b f) (Three a' b' c) = Three (mappend a a')
                                                (mappend b b')
                                                (f c)
instance (Arbitrary a, Arbitrary b, Arbitrary c) 
    => Arbitrary (Three a b c) where 
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            return $ Three a b c
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq    
type ThreeType = Three String (Sum Int) (Int, Int, Int)

-- 4
data Three' a b  = Three' a b b deriving (Show, Eq)
instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')
instance Monoid a => Applicative (Three' a) where
    pure x = Three' mempty x x
    (<*>) (Three' a f f') (Three' a' b b') = Three' (mappend a a')
                                                    (f b) (f' b')
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        b' <- arbitrary
        return $ Three' a b b'
instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq
type Three'Type = Three' String (Int,Int,Int)

-- 5
data Four a b c d = Four a b c d deriving (Show, Eq)
instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)
instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure = Four mempty mempty mempty 
    (<*>) (Four a b c f) (Four a' b' c' d) = Four (mappend a a')
                                                  (mappend b b')
                                                  (mappend c c')
                                                  (f d)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => Arbitrary (Four a b c d) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            d <- arbitrary
            return $ Four a b c d
instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq
type FourType = Four String (Sum Int) Ordering (Int, Int, Int)

-- 6
data Four' a b = Four' a a a b deriving (Show, Eq)
instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)
instance (Monoid a) => Applicative (Four' a) where
    pure = Four' mempty mempty mempty
    (<*>) (Four' a b c f) (Four' a' b' c' d) = Four' (mappend a a')
                                                     (mappend b b')
                                                     (mappend c c')
                                                     (f d)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        a' <- arbitrary
        a'' <- arbitrary
        b <- arbitrary
        return $ Four' a a' a'' b
instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq
type Four'Type = Four' String (Int,Int,Int)

main :: IO ()
main = do
    quickBatch (applicative (undefined :: PairType))
    quickBatch (applicative (undefined :: TwoType))
    quickBatch (applicative (undefined :: ThreeType ))
    quickBatch (applicative (undefined :: Three'Type))
    quickBatch (applicative (undefined :: FourType))
    quickBatch (applicative (undefined :: Four'Type))