module Vigenere where

    import Data.Char

    alphaIndex :: Char -> Int
    alphaIndex c
        | elem c ['a'..'z'] = ord c - ord 'a'
        | elem c ['A'..'Z'] = ord c - ord 'A'
        | otherwise = 0


    shift :: Char -> Char -> Char
    shift c k
        | elem c ['a'..'z'] = go c k 'a'
        | elem c ['A'..'Z'] = go c k 'A'
        | otherwise = c
        where go p key base = chr $ (mod rel r) + b
                 where rel = alphaIndex p + alphaIndex key
                       r = 26
                       b = ord base

    -- nice solution, but maps keyword to non-alpha characters
    -- e.g. MEET_AT_DAWN 
    --      ALLYALLYALLY
    vigenere' :: [Char] -> [Char] -> [Char]
    vigenere' xs ys = zipWith shift xs ((concat . repeat) ys)

    -- wrote own zipWith variant which maps only when isAlpha
    vigenere :: [Char] -> [Char] -> [Char]
    vigenere xs [] = xs -- necessary to avoid bottom
    vigenere xs ys = myZipWith shift xs ys
        where myZipWith _ [] _  = []
              myZipWith f s [] = myZipWith f s ys
              myZipWith f (a:as) k@(b:bs) =
                if isAlpha a
                then f a b : myZipWith f as bs
                else     a : myZipWith f as k