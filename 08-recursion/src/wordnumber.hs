module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = ""

digits :: Int -> [Int]
digits n
    | d == 0 = [r]
    | otherwise = digits d ++ [r]
    where (d,r) = divMod (abs n) 10

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits