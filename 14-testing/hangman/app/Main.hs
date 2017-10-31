module Main where

  import Hangman
  import Data.Char (toLower)
      
  main :: IO ()
  main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle