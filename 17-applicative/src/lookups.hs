module Lookups where

import Data.List (elemIndex)

xs :: [Integer]
xs = [1,2,3]
ys :: [Integer]
ys = [4,5,6]

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip xs ys)

y :: Maybe Integer
y = lookup 3 $ zip xs ys

z :: Maybe Integer
z = lookup 2 $ zip xs ys

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3
x' :: Maybe Int
x' = elemIndex 3 ([1,2,3,4,5] :: [Integer])

y' :: Maybe Int
y' = elemIndex 4 ([1,2,3,4,5] :: [Integer])

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'

-- 4
x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = pure sum <*> ((,) <$> x'' <*> y'')