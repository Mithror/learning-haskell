module Func where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x) == (fmap g . fmap f $ x)

-- 1
newtype Identity a = Identity a deriving (Eq, Show)
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a
type IdentId = Identity Int -> Bool
type IdentComp = Identity Int -> Fun Int String -> Fun String Float -> Bool

-- 2
data Pair a = Pair a a deriving (Eq, Show)
instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')
instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        a' <- arbitrary
        return $ Pair a a'
type PairId = Pair Int -> Bool
type PairComp = Pair Int -> Fun Int String -> Fun String Float -> Bool

-- 3
data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b
type TwoId = Two Int String -> Bool
type TwoComp = Two Int String -> Fun String Int -> Fun Int Float -> Bool 

-- 4
data Three a b c = Three a b c deriving (Eq, Show)
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)
instance (Arbitrary a, Arbitrary b, Arbitrary c) 
    => Arbitrary (Three a b c) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            return $ Three a b c
type ThreeId = Three Int String Float -> Bool
type ThreeComp = Three Int String Float ->
                 Fun Float String ->
                 Fun String Int ->
                 Bool

-- 5
data Three' a b = Three' a b b deriving (Eq, Show)
instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        b' <- arbitrary
        return $ Three' a b b'
type Three'Id = Three' Int String -> Bool
type Three'Comp = Three' Int String ->
                  Fun String Float ->
                  Fun Float Int ->
                  Bool

-- 6
data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => Arbitrary (Four a b c d) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            d <- arbitrary
            return $ Four a b c d
type FourId = Four Int String Float Ordering -> Bool
type FourComp = Four Int String Float Ordering ->
                 Fun Ordering Int ->
                 Fun Int String ->
                 Bool

-- 7
data Four' a b = Four' a a a b deriving (Eq, Show)
instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        a' <- arbitrary
        a'' <- arbitrary
        b <- arbitrary
        return $ Four' a a' a'' b
type Four'Id = Four' Int String -> Bool
type Four'Comp = Four' Int String ->
                 Fun String Bool ->
                 Fun Bool Float ->
                 Bool
                
-- 8
-- This is not possible because Trivial is of kind *
-- And you need * -> *.

main :: IO ()
main = do
    putStrLn "Functor Identity Tests"
    quickCheck (functorIdentity :: IdentId)
    quickCheck (functorIdentity :: PairId)
    quickCheck (functorIdentity :: TwoId)
    quickCheck (functorIdentity :: ThreeId)
    quickCheck (functorIdentity :: Three'Id)
    quickCheck (functorIdentity :: FourId)
    quickCheck (functorIdentity :: Four'Id)

    putStrLn "Functor Composition Tests"
    quickCheck (functorCompose' :: IdentComp)
    quickCheck (functorCompose' :: PairComp)
    quickCheck (functorCompose' :: TwoComp)
    quickCheck (functorCompose' :: ThreeComp)
    quickCheck (functorCompose' :: Three'Comp)
    quickCheck (functorCompose' :: FourComp)
    quickCheck (functorCompose' :: Four'Comp)