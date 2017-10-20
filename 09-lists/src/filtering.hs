module Filtering where

    myFilter :: Integral a => [a] -> [a]
    myFilter = filter (\x -> rem x 3 == 0)

    myFilterLength :: Integral a => [a] -> Int
    myFilterLength = length . myFilter

    myFilter' :: [Char] -> [[Char]]
    myFilter' = filter 
                 (\x -> case x of
                            "the" -> False
                            "a"   -> False
                            "an"  -> False
                            _     -> True
                )
                . words

