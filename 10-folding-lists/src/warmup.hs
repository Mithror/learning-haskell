module WarmUp where

    -- 1
    stops :: [Char]
    stops  = "pbtdkg"
    vowels :: [Char]
    vowels = "aeiou"

    stopVowelStop :: [(Char, Char, Char)]
    stopVowelStop = [(x,y,z) | x <- stops, y <- vowels, z <- stops]
    
    svsP :: [(Char, Char, Char)]
    svsP = [(x,y,z) | x <- stops, y <- vowels, z <- stops, x == 'p']
    -- or
    svsP' :: [(Char, Char, Char)]
    svsP' = [('p', x, y) | x <- vowels, y <- stops]
    -- or
    svsP'' :: [(Char, Char, Char)]
    svsP'' = filter (\(x,_,_) -> x == 'p') stopVowelStop

    nouns :: [[Char]]
    nouns = ["apple", "dog", "door", "water", "life"]

    verbs :: [[Char]]
    verbs = ["eat", "run", "drop", "jump", "grow"]

    tupleUp :: [a] -> [b] -> [c] -> [(a,b,c)]
    tupleUp xs ys zs = [(x,y,z) | x <- xs, y <- ys, z <- zs]

    nvn :: [([Char], [Char], [Char])]
    nvn = tupleUp nouns verbs nouns

    -- 2
    seekritFunc :: [Char] -> Int
    seekritFunc x = div (sum (map length (words x)))
                        (length (words x))
    -- This function returns average number of characters per word

    -- 3
    seekritFunc' :: Fractional a => [Char] -> a
    seekritFunc' x = (/) (fromIntegral $ sum (map length (words x)))
                         (fromIntegral $ length (words x))


