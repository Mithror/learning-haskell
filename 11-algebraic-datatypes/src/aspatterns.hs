module AsPatters where

    import Data.Char

    isSubseqOff :: (Eq a) => [a] -> [a] -> Bool
    isSubseqOff [] _ = True
    isSubseqOff _ [] =  False
    isSubseqOff s1@(x:xs) (y:ys) =
        if x == y
        then isSubseqOff xs ys
        else isSubseqOff s1 ys

    capitalizeWords :: String -> [(String, String)]
    capitalizeWords = map f . words
            where f [] = ([],[])
                  f s@(x:xs) = (s, toUpper x : xs)