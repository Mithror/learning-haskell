module Char where

    import Data.Char

    -- 2
    filterUpper :: [Char] -> [Char]
    filterUpper = filter isUpper

    -- 3
    upFirst :: [Char] -> [Char]
    upFirst [] = []
    upFirst (x:xs) = toUpper x : xs

    -- 4
    upAll :: [Char] -> [Char]
    upAll [] = []
    upAll (x:xs) = toUpper x : upAll xs

    -- 5
    getUppedFirst :: String -> Char
    getUppedFirst s = toUpper $ head s

    -- 6
    getUppedFirst' :: String -> Char
    getUppedFirst' = toUpper . head