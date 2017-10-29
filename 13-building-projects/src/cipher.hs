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
        | elem c ['a'..'z'] = chr (i 'a')
        | elem c ['A'..'Z'] = chr (i 'A')
        | otherwise = c
        where i b  = (mod (ci b) r) + (ord b)
              ci b = f ((ord c) - (ord b)) k
              r = 26

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

    -- 1
    -- Not a lot of error checking, but just testing
    -- reading the input
    callCaesar :: IO ()
    callCaesar = do
        putStrLn "Enter the plain text:"
        s <- getLine
        putStr "Enter the key (Char): "
        c <- getChar
        putStr "\n"
        putStrLn $ caesar s $ alphaIndex c

    callVigenere :: IO ()
    callVigenere = do
        putStrLn "Enter the plain text:"
        s <- getLine
        putStr "Enter the key (String): "
        k <- getLine
        putStr "\n"
        putStrLn $ vigenere s k