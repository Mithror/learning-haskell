module Chapter4 where

-- Chapter Exercises
-- 8
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

-- 9
myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x

-- 10
f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f x y = ((snd x, snd y), (fst x, fst y))

-- Correcting Syntax
-- 1
myF :: String -> Int
myF xs = w + 1
    where w = length xs

-- 2
myId :: a -> a
myId x = x

-- 3
myFirst :: (a,b) -> a
myFirst (a, _) = a
-- or myFirst (a, b) = a
-- or myFirst t = fst t