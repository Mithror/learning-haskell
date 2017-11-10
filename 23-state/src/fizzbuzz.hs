module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State
-- http://hackage.haskell.org/package/dlist
-- import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise = show n

-- fizzbuzzlist :: [Integer] -> DL.DList String
-- fizzbuzzlist list = execState (mapM_ addResult list) DL.empty

-- addResult :: Integer -> State (DL.DList String) ()
-- addResult n = do
--     xs <- get
--     let result = fizzBuzz n
--     put (DL.snoc xs result)

-- main :: IO ()
-- main = mapM_ putStrLn $ fizzbuzzlist [1..100]

fizzbuzzlist :: [Integer] -> [String]
fizzbuzzlist list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

-- they all seem to take about the same amount of time...
main :: IO ()
-- main = mapM_ putStrLn $ reverse $ fizzbuzzlist [1..100]
main = mapM_ putStrLn $ fizzbuzzFromTo 1 500000
-- main = mapM_ putStrLn $ fizzbuzzFromTo' 1 500000

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = go from to []
    where go f t st
            | f >= t = st
            | otherwise = execState (addResult f) (go (f+1) t st)

fizzbuzzFromTo' :: Integer -> Integer -> [String]
fizzbuzzFromTo' from to = execState (mapM_ addResult list) []
    where list = go from to []
          go f t l
                | f >= t = l
                | otherwise = go (f+1) t (f:l)