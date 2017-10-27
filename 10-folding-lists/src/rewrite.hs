module Rewrite where

    -- 1
    myOr :: [Bool] -> Bool
    myOr = foldr (||) False

    -- 2
    myAny :: (a -> Bool) -> [a] -> Bool
    myAny f = foldr (\a b -> f a || b) False

    -- 3
    myElem :: Eq a => a -> [a] -> Bool
    myElem x = foldr (\a b -> (x == a) || b) False

    myElem' :: Eq a => a -> [a] -> Bool
    myElem' x = any (x==)

    -- 4
    myReverse :: [a] -> [a]
    myReverse = foldr (\a b -> b ++ [a]) []
    -- other solution would be to use foldl (flip (:)) []

    -- 5
    myMap :: (a -> b) -> [a] -> [b]
    myMap f = foldr (\a b -> f a : b) []
    -- myMap = foldr ((:) . f) []

    -- 6  
    myFilter :: (a -> Bool) -> [a] -> [a]
    myFilter f = foldr g []
        where g a b
                | f a = a : b
                | otherwise = b

    -- 7
    squish :: [[a]] -> [a]
    squish = foldr (++) []
    
    -- 8
    squishMap :: (a -> [b]) -> [a] -> [b]
    squishMap f = foldr ((++) . f) []

    -- 9
    squishAgain :: [[a]] -> [a]
    squishAgain = squishMap id

    -- 10
    myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
    myMaximumBy _ [] = undefined
    myMaximumBy _ [x] = x
    myMaximumBy f (x:xs) = foldl go x xs
        where go a b
               | f a b == GT = a
               | otherwise   = b

    -- 11
    myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
    myMinimumBy _ [] = undefined
    myMinimumBy _ [x] = x
    myMinimumBy f (x:xs) = foldl go x xs
        where go a b
               | f a b == LT = a
               | otherwise   = b