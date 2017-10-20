module Split where

-- Technically words and lines do not do the same thing. E.g
-- unwords . words $ " one   two  three  " == "one two three"
-- unlines . lines $ "\none\n\ntwo\three"  == "\none\n\ntwo\three\n"
-- i.e. words removes all spaces, lines splits between each '\n'
-- put otherwise: there are no empty words, but there are empty lines,
--                though there is no last empty line
-- The last part is evident by: lines "a\n" == ["a"] and lines "a" = ["a"]

-- The function here sees no empty words nor does it see empty lines
mySplit :: [Char] -> Char -> [[Char]]
mySplit [] _ = []
mySplit s@(x:xs) c
    | x == c    = mySplit xs c
    | otherwise = [part1] ++ (mySplit part2 c)
    where part1 = takeWhile b s
          part2 = dropWhile b s
          b     = (/=) c

myWords :: [Char] -> [[Char]]
myWords = flip mySplit $ ' '

myLines :: [Char] -> [[Char]]
myLines = flip mySplit $ '\n'