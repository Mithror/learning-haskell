module Words where

myWords :: String -> [String]
myWords [] = [] 
myWords (' ' : xs) = myWords xs
myWords xs = [takeWhile b xs] ++ (myWords (dropWhile b xs))
    where b = (/=) ' '