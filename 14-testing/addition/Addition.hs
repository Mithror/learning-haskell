-- Addition.hs
module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello"

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            ((1 :: Integer) + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            ((2 :: Integer) + 2) `shouldBe` 4
        it "15 divided by 3 is 5" $ do
            dividedBy (15 :: Integer) 3 `shouldBe` (5,0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy (22 :: Integer) 5 `shouldBe` (4,2)
        -- Intermission: Short Exercise
        it "0 times 1 is 0" $ do
            myMult (0 :: Integer) 1 `shouldBe` 0
        it "1 times 0 is 0" $ do
            myMult (1 :: Integer) 0 `shouldBe` 0
        it "2 times 2 is 4" $ do
            myMult (2 :: Integer) 2 `shouldBe` 4
        it "2 times (-2) is (-4)" $ do
            myMult (2 :: Integer) (-2) `shouldBe` (-4)
        it "(-2) times 2 is (-4)" $ do
            myMult (-2 :: Integer) 2 `shouldBe` (-4)
        it "(-2) times (-2) is 4" $ do
            myMult (-2 :: Integer) (-2) `shouldBe` 4
        -- QuickCheck
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)


dividedBy :: Integral a => a -> a -> (a,a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

-- Intermission: Short Exercise
myMult :: (Eq a, Num a) => a -> a -> a
myMult 0 _ = 0
myMult _ 0 = 0
myMult a b = 
    if abs b == b
    then a + myMult a (b - 1)
    else negate $ myMult a (negate b)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

oneThroughThree' :: Gen Int
oneThroughThree' = elements [1, 2, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a,b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a,b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a,b,c)
genThreeple = do 
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do 
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

genMaybe :: (Arbitrary a) => Gen (Maybe a)
genMaybe = do 
    a <- arbitrary
    elements [Nothing, Just a]

genMaybe' :: (Arbitrary a) => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [ (1, return Nothing), (3, return (Just a)) ]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater