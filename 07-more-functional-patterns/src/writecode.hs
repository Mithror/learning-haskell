module WriteCode where

-- 1a
tensDigit :: Integral a => a -> a
tensDigit = snd . (flip divMod) 10
-- 1b This has the same type.
-- 1c
hunsD :: Integral a => a -> a
hunsD = snd . (flip divMod) 100

-- 2
foldBool1 :: a -> a -> Bool -> a
foldBool1 x y b =
    case b of
        False -> x
        True -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
    | b == False = x
    | otherwise = y

-- 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

-- 4
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main :: IO ()
main = do
    print ((roundTrip 4) :: Integer)
    print (id 4 :: Integer)

-- 5
roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

-- 6
roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' = read . show
-- roundTrip'' 4 :: Integer