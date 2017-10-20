module DividedBy where

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
           | n < d = (count, n)
           | otherwise = go (n - d) d (count + 1)

-- For this exercise, we'll revert to the Integer -> Integer -> Integer type.
-- This, to be able to use the hint provides which uses Integer
data DividedResult = 
    Result Integer
  | DividedByZero
  deriving (Show)

dividedBy' :: Integer -> Integer -> DividedResult
dividedBy' _ 0 = DividedByZero
dividedBy' num denom = -- is actually the same as the regular function.
    Result . toInteger . fst $ dividedBy num denom