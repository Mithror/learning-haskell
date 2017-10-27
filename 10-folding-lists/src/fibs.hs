module Fibs where

    fibs :: [Integer]
    fibs = 1 : scanl (+) 1 fibs

    -- To understand how this works, let's expand fibs, but first
    -- the definition of scanl
    --
    scanl' :: (a -> b -> a) -> a -> [b] -> [a]
    scanl' f q ls =
      q : (case ls of
             [] -> []
             x:xs -> scanl' f (f q x) xs)

    -- Now on to the expansion, replace fibs with it's definition
    fibs1 :: [Integer]
    fibs1 = 1 : scanl (+) 1 (1 : scanl (+) 1 fibs)

    -- Apply definition of scanl
    fibs2 :: [Integer]
    fibs2 = 1 : (1 : scanl (+) 2 (scanl (+) 1 fibs))
    
    -- As you can see we've constructed another element of the list
    -- If we continue, we'll generate another one, but before we can do this
    -- we have to repalce fibs with it's definition again.
    -- Note: all fibs's are the same so you replace another fibs instead
    fibs3 :: [Integer]
    fibs3 = 1 : (1 : scanl (+) 2 (scanl (+) 1 
                 (1 : scanl (+) 1 fibs)))
    
    -- Apply the definition of the second scanl
    fibs4 :: [Integer]
    fibs4 = 1 : (1 : scanl (+) 2 (1 : scanl (+) 2 (scanl (+) 1 fibs)))

    -- Apply the definition of the first scanl
    fibs5 :: [Integer]
    fibs5 = 1 : (1 : (2 : (scanl (+) 3 (scanl (+) 2 (scanl (+) 1 fibs)))))

    -- We have now generated the first 3 elements of the fibonacci series
    -- If the same replacements are being applied, it will keep generating
    -- new entries.
    
    fibsN :: Int -> Integer
    fibsN = (!!) fibs
    
    -- 1
    fibs' :: [Integer]
    fibs' = take 20 $ fibs

    -- 2
    fibs'' :: [Integer]
    fibs'' = takeWhile (<100) fibs

    -- 3
    fact :: [Integer]
    fact =  scanl (*) 1 [1..]

    factN :: Int -> Integer
    factN = (!!) fact
