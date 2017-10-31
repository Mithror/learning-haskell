module Idempotence where

import Test.QuickCheck
import Data.List (sort)
import Data.Char (toUpper)

twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = (toUpper x) : xs

-- 1
prop_cap :: String -> Bool
prop_cap s = (capitalizeWord s == twice capitalizeWord s) 
             &&
             (capitalizeWord s == fourTimes capitalizeWord s)
-- 2
prop_sort :: (Ord a, Eq a) => [a] -> Bool
prop_sort xs = (sort xs == twice sort xs)
               &&
               (sort xs == fourTimes sort xs)

main :: IO ()
main = do
    quickCheck prop_cap
    quickCheck (prop_sort :: String -> Bool)