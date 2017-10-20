module SquareCube where

    mySqr :: (Enum a, Num a) => [a]
    mySqr = [x^(2 :: Integer) | x <- [1..5]]

    myCube :: (Enum a, Num a) => [a]
    myCube = [y^(3 :: Integer) | y <- [1..5]]

    myTuples :: (Enum a, Num a) => [(a,a)]
    myTuples = [(x,y) | x <- mySqr, y <- myCube]

    myTuples' :: (Ord a, Enum a, Num a) => [(a,a)]
    myTuples' = [(x,y) | x <- mySqr, y <- myCube,
                         x < 50, y < 50]

    myTuples'Length :: Int
    myTuples'Length = length (myTuples' :: [(Int,Int)])
    