module WarmingUp where

import Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char],[Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
    a <- cap
    b <- rev
    return (a, b)

tupled'' :: [Char] -> ([Char],[Char])
tupled'' = cap >>= \a -> rev >>= \b -> return (a,b)
