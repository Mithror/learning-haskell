module DefineFunc where

-- 1
i :: a -> a
i x = x;

-- 2
c :: a -> b -> a
c x _ = x

-- 3
c'' :: b -> a -> b
c'' x _ = x
-- Yes they are the same thing

-- 4
c' :: a -> b -> b
c' _ y = y

-- 5
r :: [a] -> [a]
--r (_:xs) = xs
r (x:xs) = xs ++ [x]
-- any many other options...

-- 6
co :: (b -> c) -> (a -> b) -> a -> c
co = (.)
-- co bToC aToB a = bToC (aToB a)
-- co f    g    a = (f . g) a
-- co f    g      = (f . g)
-- These are all the same.

-- 7
a :: (a -> c) -> a -> a
a _ x = x

-- 8
a' :: (a -> b) -> a -> b
a' = ($)
-- a f x = f x
-- a f = f