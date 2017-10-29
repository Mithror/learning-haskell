module StringProc where

    -- 1
    notThe :: String -> Maybe String
    notThe "the" = Nothing
    notThe s = Just s

    replaceThe :: String -> String
    replaceThe = unwords . fmap rep . fmap notThe . words
        where rep Nothing = "a"
              rep (Just t) = t

    -- 2
    startsWithVowel :: String -> Bool
    startsWithVowel "" = False
    startsWithVowel (c:_) = elem c "aeiou"

    -- Probably better ways of doing this
    -- but wanted to see if this was actually
    -- possible!
    countTheBeforeVowel :: String -> Integer
    countTheBeforeVowel = 
        toInteger . length . f .
        (fmap .fmap) startsWithVowel . 
        fmap notThe . words
        where f [] = []
              f (_:[]) = []
              f ((Just _):xs) = f xs
              f (Nothing:Nothing:xs) = f (Nothing:xs)
              f (Nothing:Just False:xs) = f xs
              f (Nothing:Just True:xs) = [True] ++ f xs
              
    -- 3
    isVowel :: Char -> Bool
    isVowel = (flip elem) "aeiou"

    countVowels :: String -> Integer
    countVowels = toInteger . length . filter isVowel