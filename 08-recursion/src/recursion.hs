module Recursion where

-- 2
recsum :: (Eq a, Num a) => a -> a
recsum 1 = 1
recsum n = n + (recsum (n -1))
-- The above function does not really work with negative numbers but we can't
-- check for negativity because this would require the Ord typeclass.
-- You can use abs however
recsum' :: (Eq a, Num a) => a -> Maybe a
recsum' n
    | isNegative n = Nothing
    | n == 0 = Just 0
    | otherwise = go (+n) (recsum' (n - 1))
    where go f (Just a) = Just (f a)
          go _ _ = Nothing
-- Or: | otherwise = fmap (+n) (recsum' (n - 1))
    

-- 3
myMult :: (Integral a) => a -> a -> a
myMult 0 _ = 0
myMult _ 0 = 0
myMult x y
    | y >= 0 = go x y
    -- if y is negative we'll just treat y as positive and negate the result.
    -- x * (-y) = - (x * y)
    | otherwise = negate $ go x $ negate y
    where go a 1 = a
          go a b = a + (go a (b-1))

-- helper functions
isPositive :: (Eq a, Num a) => a -> Bool
isPositive n = 
    n == abs n

isNegative :: (Eq a, Num a) => a -> Bool
isNegative = not . isPositive