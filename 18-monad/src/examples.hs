module Examples where

import Control.Monad (join)

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x,x*x]
        else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs =
    xs >>= \x -> if even x then [x*x,x*x] else [x*x]

twiceWhenEven'' :: [Integer] -> [Integer]
twiceWhenEven'' xs =
     join (fmap (\x -> if even x then [x*x,x*x] else [x*x]) xs)


data Cow = Cow { name :: String, age :: Int, weight :: Int }
           deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing
            
weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
    in if (n == "Bess") && (w > 499)
       then Nothing
       else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
    case noEmpty name' of
        Nothing -> Nothing
        Just n ->
            case noNegative age' of
                Nothing -> Nothing
                Just a ->
                    case noNegative weight' of
                        Nothing -> Nothing
                        Just w -> weightCheck $ Cow n a w

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
    n <- noEmpty name'
    a <- noNegative age'
    w <- noNegative weight'
    weightCheck $ Cow n a w

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
    noEmpty name' >>= \n ->
        noNegative age' >>= \a ->
            noNegative weight'>>= \w ->
                weightCheck $ Cow n a w

mkSphericalCow''' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow''' name' age' weight' =
    join (fmap f $ noEmpty name')
    where f n = join (fmap g $ noNegative age')
              where g a = join (fmap h $ noNegative weight')
                        where h w = weightCheck $ Cow n a w 

-- Some explanation:
-- Starting at the end with the function h:
-- h :: Int -> Maybe Cow

-- We know g takes and Int (as the `a` given is used in the construction of
-- Cow). `h` is fmapped over a Maybe Int, giving us a Maybe (Maybe Cow)
-- (fmap :: (Int -> Maybe Cow) -> Maybe Int -> Maybe (Maybe Cow))
-- Using join, reduces this to just a Maybe Cow, so this makes:
-- g :: Int -> Maybe Cow

-- And looking at f, we see that it takes a String (n is used as String).
--  `g` is mapped over a Maybe Int, giving us a Maybe (Maybe Cow))
-- (fmapp :: (Int -> Maybe Cow) -> Maybe Int -> Maybe (Maybe Cow))
-- Using join reduces this to just a Maybe Cow to, so we have:
-- f :: String -> Maybe Cow

-- Finally `f` is mapped over a Maybe String, giving us a Maybe (Maybe Cow)
-- (fmap :: (String -> Maybe Cow) -> Maybe String -> Maybe (Maybe Cow))
-- Using join, reduces this to jsut a Maybe Cow, this gives us the
-- Maybe Cow
                        
-- mkSphericalCow''' :: String -> Int -> Int -> Maybe Cow
-- mkSphericalCow''' name' age' weight' = z name' age' weight'
--     where z :: String -> Int -> Int -> Maybe Cow
--           z n a w = join (fmap (f w a) $ noEmpty n)
--           f :: Int -> Int -> String -> Maybe Cow
--           f w a n = join (fmap (g w n) $ noNegative a)
--           g :: Int -> String -> Int -> Maybe Cow
--           g w n a = join (fmap (h n a) $ noNegative w)
--           h :: String -> Int -> Int -> Maybe Cow
--           h n a w = weightCheck $ Cow n a w

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i = if even i then (Just (i+1)) else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

doSomething' :: Integer -> Maybe (Integer, Integer, String)
doSomething' n = do
    a <- f n
    b <- g a
    c <- h b
    pure (a, b, c)

doSomething'' :: Integer -> Maybe (Integer, Integer, String)
doSomething'' n =
    (pure g) <*> (f n)