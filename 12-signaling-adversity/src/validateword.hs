module ValidateWord where

    import Data.List

    newtype Word' = Word' String deriving (Eq, Show)

    vowels :: String
    vowels = "aeiou"

    isVowel :: Char -> Bool
    isVowel = (flip elem) vowels

    mkWord :: String -> Maybe Word'
    mkWord s = check $ partition isVowel s
        where check (xs,ys)
                | length xs >= length ys = Nothing
                | otherwise = Just $ Word' s