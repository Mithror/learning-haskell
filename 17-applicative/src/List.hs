module List where 

import Data.Functor ((<$>))
import Data.Monoid ((<>))

import Test.QuickCheck.Gen (sized)
import Control.Applicative (liftA2)

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

-- My original implementation (before looking at the list)
-- instance Applicative List where
--     pure a = Cons a Nil
--     _ <*> Nil = Nil -- strictly not neccessary but slightly faster?
--                     -- if the List is empty then the last pattern mach
--                     -- will be 
--                     -- (<>) ((<$>) f Nil) (fl <*> Nil)
--                     -- (<>) Nil ((<>) ((<$>) f Nil) (fl' <> Nil))
--                     -- Which will continue until fl/fl' is Nil and which
--                     -- will pattern match with second pattern match

--     Nil <*> _ = Nil
--     (Cons f fl) <*> l = 
--         (<>) ((<$>) f l) (fl <*> l)


instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a l) = Cons (f a) (f <$> l)

instance Monoid (List a) where
    mempty = Nil
    mappend Nil l = l
    mappend l Nil = l
    mappend (Cons a al) l = Cons a $ al  <> l

-- The append function from the book is just (<>)
-- from the Monoid typeclass
-- append :: List a -> List a -> List a
-- append Nil ys = ys
-- append (Cons x xs) ys = Cons x $ xs `append` ys

-- We probably could also implemnt the Foldable typeclass, so
-- we can start using foldr, but this _is_ foldr :)
fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold (<>) Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = (concat' . (<$>) f)


-- Imagine we have a List of functions `fs` and a List of elements `as`
-- For each f in fs we want to fmap it over al and append the result.
-- We can use the now defined flatMap for this, but we should figure
-- out what is what. (a -> List b) -> List a -> List b

-- Option 1
-- If `List a` is the `as` then (a -> List b) should be a function
-- that takes an `a` and has all functions applied to it
-- This would be akin to: fmap ($a) fs 

-- instance Applicative List where
--     pure = flip Cons Nil
--     fs <*> xs = flatMap (\x -> ($x) <$> fs) xs

-- But this doesn't look so nice. We could look at it slightly different
-- Option 2
-- If `List a` is the `fs` then (a -> List b) should be a function that
-- takes an `f` and applies it to all `as`.
-- This would be fmap f as or \f -> fmap f xs or (`fmap` xs) 
instance Applicative List where
    pure = flip Cons Nil
    fs <*> xs = flatMap (<$> xs) fs

-- First attempt at Arbitrary for List
-- Slow for the composition test
fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a l) = a : fromList l

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        as <- listOf arbitrary
        return $ toList as

-- Have to find a good balance for Nil
-- instance Arbitrary a => Arbitrary (List a) where
--     arbitrary = do
--         a <- arbitrary
--         l <- arbitrary
--         frequency [(9,return $ Cons a l), (1, return Nil)]

-- Peaking at arbitrary for [], also long which made my first
-- version actually pretty ok! The difference is probably in the
-- size of lists generated between this and the other frequency one

-- replicateM' :: (Applicative m) => Int -> m a -> m (List a)
-- replicateM' cnt0 f =
--     loop cnt0
--   where loop cnt
--           | cnt <= 0 = pure Nil
--           | otherwise = liftA2 Cons f (loop (cnt - 1))

-- vectorOf' :: Int -> Gen a -> Gen (List a)
-- vectorOf' = replicateM'

-- listOf' :: Gen a -> Gen (List a)
-- listOf' gen = sized $ \n ->
--     do k <- choose (0,n)
--        vectorOf' k gen


-- instance Arbitrary a => Arbitrary (List a) where
--     arbitrary = listOf' arbitrary

instance Eq a => EqProp (List a) where
    (=-=) = eq

-- take' :: Int -> List a -> List a
-- take' = undefined

-- newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

-- instance Eq a => EqProp (ZipList' a) where
--     xs =-= ys = xs' `eq` ys'
--       where xs' = let (ZipList' l) = xs
--                   in take' 3000 l
--             ys' = let (ZipList' l) = ys
--                   in take' 3000 l

-- instance Functor ZipList' where
--     (<$>) f (ZipList' xs) = ZipList' $ (<$>) f xs

-- instance Applicative ZipList' where
--     pure a = ZipList' (Cons a Nil)
--     (ZipList' Nil) <*> _ = ZipList' Nil
--     _ <*> (ZipList' Nil) = ZipList' Nil
--     (ZipList' (Cons f fl)) <*> (ZipList' (Cons a as)) =
--         ZipList' $ Cons (f a) (recurse fl as)
--         where recurse fl' as' =
--                 let (ZipList' z) = ((ZipList' fl') <*> (ZipList' as'))
--                 in z

main :: IO ()
main = do
    quickBatch (monoid (Nil :: List Int))
    quickBatch (applicative (Nil :: List (Int,Int,Int)))