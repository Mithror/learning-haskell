module Arith3Broken where

main :: IO ()
-- Main should be main
main = do
    print (1 + 2) -- added braces
    putStrLn "10" -- must be String
    print (negate (-1)) -- or negate 1
    print ((+) 0 blah)
      where blah = negate 1