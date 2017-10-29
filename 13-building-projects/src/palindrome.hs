module Palindrome where

import Control.Monad
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case (pal line1) of
        True -> putStrLn "It's a palindrome!"
        False -> putStrLn "Nope!"
    where pal s = 
           let s' = map toLower $ filter isAlpha s
           in s' == reverse s'
        
