module Standards where

    -- 1
    myOr :: [Bool] -> Bool
    myOr [] = True
    myOr (x:xs) = x || myOr xs

    -- 2
    myAny :: (a -> Bool) -> [a] -> Bool
    myAny _ [] = True
    myAny f (x:xs) = f x || myAny f xs

    -- 3
    myElem :: Eq a => a -> [a] -> Bool
    myElem _ [] = False
    myElem a (x:xs) = (a == x) || myElem a xs

    myElem' :: Eq a => a -> [a] -> Bool
    myElem' a = any (a==)

    -- 4
    myReverse :: [a] -> [a]
    myReverse [] = []
    myReverse (x:xs) = myReverse xs ++ [x]

    -- 5
    squish :: [[a]] -> [a]
    squish [] = []
    squish (x:xs) = x ++ squish xs

    -- 6
    squishMap :: (a -> [b]) -> [a] -> [b]
    squishMap _ [] = []
    squishMap f (x:xs) = f x ++ squishMap f xs

    -- 7
    squishAgain :: [[a]] -> [a]
    squishAgain = squishMap id
    -- squishAgain = squishMap ([]++)
    
    -- 8
    myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
    myMaximumBy _ [] = undefined
    myMaximumBy _ (x:[]) = x
    myMaximumBy f (x:xs) = 
        case f x y of
            LT -> y
            _  -> x
        where y = myMaximumBy f xs

    -- 9
    myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
    myMinimumBy _ [] = undefined
    myMinimumBy _ (x:[]) = x
    myMinimumBy f (x:xs) =
        case f x y of
            LT -> x
            _  -> y
        where y = myMinimumBy f xs

    -- 10
    myMaximum :: (Ord a) => [a] -> a
    myMaximum = myMaximumBy compare

    myMinimum :: (Ord a) => [a] -> a
    myMinimum = myMinimumBy compare
                            