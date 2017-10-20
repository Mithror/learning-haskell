module EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
-- eftBool False True  = [False, True]
-- eftBool False False = [False]
-- eftBool True  False = []
-- eftBool True  True  = [True]
-- eftBool a b
--     | a > b = []
--     | a == b = [a]
--     | otherwise = [a] ++ (eftBool (succ a) b)
eftBool = helper


eftOrd :: Ordering -> Ordering -> [Ordering]
-- eftOrd a b
--     | a > b = []
--     | a == b = [a]
--     | otherwise = [a] ++ (eftOrd (succ a) b)
eftOrd = helper

eftInt :: Int -> Int -> [Int]
-- eftInt a b
--     | a > b = []
--     | a == b = [a]
--     | otherwise = [a] ++ (eftInt (succ a) b)
eftInt = helper

eftChar :: Char -> Char -> [Char]
-- eftChar a b
--     | a > b = []
--     | a == b = [a]
--     | otherwise = [a] ++ (eftChar (succ a) b)
eftChar = helper

helper :: Enum a => a -> a -> [a]
helper a b
    | ai > bi = []
    | ai == bi = [a]
    | otherwise = [a] ++ (helper (succ a) b)
    where ai = fromEnum a
          bi = fromEnum b