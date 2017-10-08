module Chapter3 where

-- Building Functions
-- Exercise 2
-- This is just the most straighforward solution. Not very flexible.
appendBang :: String -> String
appendBang x = x ++ "!"

-- Again not very flexible, nor safe. Not every string has a fourth element.
getFourth :: String -> String
getFourth x = (x !! 4) : ""

-- Also not safe as there might not be enough characters to drop
dropNine :: String -> String
dropNine x = drop 9 x

-- Exercise 3
thirdLetter :: String -> Char
thirdLetter x = x !! 2

-- Exercise 4
letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

-- Exercise 5
-- Because we only want this to work for "Curry is awesome", I've decided to
-- only implement it as such and thus it returns just a String
rvrs :: String
rvrs = 
    let s = "Curry is awesome"
        start = take 5 s          -- "Curry"
        mid   = take 4 $ drop 5 s -- " is "
        end   = drop 9 s          -- "awesome"  
    in end ++ mid ++ start
