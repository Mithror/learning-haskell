module Main where

import Data.Char
import System.Environment (getArgs)
import System.IO (hPutStr, hGetChar, stdout, stdin, hWaitForInput
                 , stderr, interact)
import System.Exit (exitFailure)
import Control.Exception (try)
import System.IO.Error (isEOFError, ioError)

alphaIndex :: Char -> Int
alphaIndex c
    | elem c ['a'..'z'] = ord c - ord 'a'
    | elem c ['A'..'Z'] = ord c - ord 'A'
    | otherwise = 0

shift :: (Int -> Int -> Int) -> Char -> Char -> Char
shift f c k
    | elem c ['a'..'z'] = go c k 'a'
    | elem c ['A'..'Z'] = go c k 'A'
    | otherwise = c
    where go p key base = let rel = f (alphaIndex p) (alphaIndex key)
                              r = 26
                              b = ord base
                          in chr $ (mod rel r) + b

-- wrote own zipWith variant which maps only when isAlpha
vigenere :: (Int -> Int -> Int) -> [Char] -> [Char] -> [Char]
vigenere _ xs [] = xs -- necessary to avoid bottom
vigenere f xs ys = myZipWith (shift f) xs ys
    where myZipWith _ [] _  = []
          myZipWith f s [] = myZipWith f s ys
          myZipWith f (a:as) k@(b:bs) =
            if isAlpha a
            then f a b : myZipWith f as bs
            else     a : myZipWith f as k

readAll :: IO String
readAll = do
    b <- try $ hWaitForInput stdin 5000
    case b of
        Left e -> if isEOFError e
                  then return ""
                  else ioError e
        Right False -> do
            hPutStr stderr $ "Timeout."
            exitFailure
        Right True -> do
            c <- hGetChar stdin
            fmap (c:) readAll

encrypt :: String -> IO ()
encrypt key = do
    s <- readAll
    let s' = vigenere (+) s key
    hPutStr stdout s'

decrypt :: String -> IO ()
decrypt key = do
    s <- readAll
    let s' = vigenere (-) s key
    hPutStr stdout s'

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-e", key] -> encrypt key
        ["-d", key] -> decrypt key
        otherwise -> putStrLn "Invalid arguments. Use (-d|-e) <key>."

anotherWay :: IO ()
anotherWay = do
    args <- getArgs
    case args of
        ["-e", key] -> interact $ flip (vigenere (+)) key
        ["-d", key] -> interact $ flip (vigenere (-)) key
        otherwise -> putStrLn "Invalid arguments. Use (-d|-e) <key>."
