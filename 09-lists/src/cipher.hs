module Cipher where

    import Data.Char

    -- Chapter 9.12
    caesar :: String -> Int -> String
    caesar [] _ = []
    caesar (x:xs) k
        | isAlpha x = go x k ++ caesar xs k
        | otherwise = caesar xs k
        where go c key = [enc c (+) key]

    unCaesar :: String -> Int -> String
    unCaesar [] _ = []
    unCaesar (x:xs) k
        | isAlpha x = go x k ++ unCaesar xs k
        | otherwise = caesar xs k
        where go c key = [enc c (-) key]
    
    enc :: Char -> (Int -> Int -> Int) -> Int -> Char
    enc c f k
        | isAlpha c = chr i
        | otherwise = c
        where i  = (mod ci r) + (ord base)
              ci = f ((ord c) - (ord base)) k
              r = 26
              base = 'a'