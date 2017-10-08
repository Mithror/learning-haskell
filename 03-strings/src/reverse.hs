module Reverse where

rvrs :: String -> String
rvrs s = end ++ mid ++ start
    where start = take 5 s          -- "Curry"
          mid   = take 4 $ drop 5 s -- " is "
          end   = drop 9 s          -- "awesome"

main :: IO ()
-- main = print (rvrs "Curry is awesome")
main = print $ rvrs "Curry is aweomse"