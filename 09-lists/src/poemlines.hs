module PoemLines where

firstSen :: [Char]
firstSen  = "Tyger Tyger, burning bright\n"
secondSen :: [Char]
secondSen = "In the forests of the night\n"
thirdSen :: [Char]
thirdSen  = "What immortal hand or eye\n"
fourthSen :: [Char]
fourthSen = "Could frame thy fearful\
           \ symmetry?"

sentences :: [Char]
sentences = firstSen ++ secondSen
         ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines [] = [] 
myLines ('\n' : xs) = myLines xs
myLines xs = [takeWhile b xs] ++ (myLines (dropWhile b xs))
    where b = (/=) '\n'