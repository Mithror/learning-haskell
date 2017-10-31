module PropTest where

import Test.QuickCheck
import Test.QuickCheck.Modifiers (NonZero)
import Test.QuickCheck.Function 
import Data.List (sort)

-- Note:
-- quickCheck takes a type of the Testable typeclass
-- Arbitrary a => a -> Bool is an instance of this, since
-- Bool is also Testable.

-- 1
half :: Fractional a => a -> a
half = (/2)
halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

prop_half :: (Eq a, Fractional a) => a -> Bool
prop_half = \x -> halfIdentity x == x

-- 2
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where go _ status@(_,False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, _) = (Just y, x >= y)
prop_sort :: Ord a => [a] -> Bool
prop_sort = listOrdered . sort

-- 3
plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

-- 4
multAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z
multCommutative :: (Num a, Eq a) => a -> a -> Bool
multCommutative x y = x * y == y * x

-- 5
prop_quotRem :: (Eq a, Integral a) => a -> a -> Bool
prop_quotRem _ 0 = True -- exclude 0 from the test
prop_quotRem x y = (quot x y)*y + (rem x y) == x
prop_divMod :: (Eq a, Integral a) => a -> a -> Bool
prop_divMod _ 0 = True -- exclude 0 from the test
prop_divMod x y = (div x y)*y + (mod x y) == x
-- other solution would be the create a generator for Int or Integers or ...
-- which excludes 0
-- OR use the modifiers from quickcheck
prop_quotRem' :: (Eq a, Integral a) => NonZero a -> NonZero a -> Bool
prop_quotRem' (NonZero x) (NonZero y) = (quot x y)*y + (rem x y) == x
prop_divMod' :: (Eq a, Integral a) => NonZero a -> NonZero a -> Bool
prop_divMod' (NonZero x) (NonZero y) = (div x y)*y + (mod x y) == x

-- 6 These will fail
prop_eAss :: (Eq a, Num a, Integral a) => a -> a -> a -> Bool
prop_eAss x y z = x ^ (y ^ z) == (x ^ y) ^ z
prop_eCom :: (Eq a, Num a, Integral a) => a -> a -> Bool
prop_eCom x y = x ^ y == y ^ x

-- 7
prop_reverse :: Eq a => [a] -> Bool
prop_reverse xs = (reverse . reverse) xs == id xs

-- 8
prop_apply :: Eq b => (Fun a b) -> a -> Bool
prop_apply (Fun _ f) a = (f $ a) == f a
prop_compose :: Eq c => (Fun b c) -> (Fun a b) -> a -> Bool
prop_compose (Fun _ f) (Fun _ g) a = (f . g) a == f (g a)

-- 9
prop_append :: Eq a => [a] -> [a] -> Bool
prop_append xs ys = foldr (:) xs ys == (++) xs ys
prop_concat :: Eq a => [[a]] -> Bool
prop_concat xs = foldr (++) [] xs == concat xs

-- 10
prop_length :: Int -> [a] -> Bool
prop_length n xs = length (take n xs) == n

-- 11
prop_readshow :: (Read a, Show a, Eq a) => a -> Bool
prop_readshow x = (read (show x)) == x

main :: IO ()
main = do
    quickCheck (prop_half :: Double -> Bool) -- Float can also be checked
    quickCheck (prop_sort :: [Int] -> Bool) -- Lots of other options available
    quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)
    quickCheck (plusCommutative :: Integer -> Integer -> Bool)
    quickCheck (multAssociative :: Int -> Int -> Int -> Bool)
    quickCheck (multCommutative :: Integer -> Integer -> Bool)
    quickCheck (prop_quotRem :: Integer -> Integer -> Bool)
    quickCheck (prop_divMod :: Integer -> Integer -> Bool)
    quickCheck (prop_quotRem' :: NonZero Integer -> NonZero Integer -> Bool)
    quickCheck (prop_divMod' :: NonZero Integer -> NonZero Integer -> Bool)
    quickCheck (prop_eAss :: Integer -> Integer -> Integer -> Bool) -- fails
    quickCheck (prop_eCom :: Integer -> Integer -> Bool) -- fails
    quickCheck (prop_reverse :: [Char] -> Bool)
    quickCheck (prop_apply :: (Fun Integer Integer) -> Integer -> Bool)
    quickCheck (prop_compose :: (Fun Int Int) -> (Fun Int Int) -> Int -> Bool)
    quickCheck (prop_append :: [Char] -> [Char] -> Bool)
    quickCheck (prop_concat :: [[Char]] -> Bool)
    quickCheck (prop_length :: Int -> [String] -> Bool)
    quickCheck (prop_readshow :: Int -> Bool)