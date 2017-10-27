module LangExerc where

    import Data.Char
    import Data.List

    -- 1
    capitalizeWord :: String -> String
    capitalizeWord [] = []
    capitalizeWord (x:xs) = toUpper x : xs

    -- 2
    capitalizeParagraph :: String -> String
    capitalizeParagraph = unwords . (foo True) . words
        where foo _ [] = []
              foo True (x:xs) = capitalizeWord x : go (x,xs)
              foo False (x:xs) = x : go (x,xs)
              go (_, []) = []
              go (x,xs) = foo (isSuffixOf "." x) xs
