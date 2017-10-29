module Main where

import Control.Monad (forever)
import Data.Char (toLower, isAlpha)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (WordList $ lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

maxWrongGuesses :: Int
maxWrongGuesses = 10

gameWords :: IO WordList
gameWords = do
  WordList aw <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w = 
          let l = length (w :: String)
          in l >= minWordLength && l < maxWordLength
             && all isAlpha w

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] Int
instance Show Puzzle where
  show (Puzzle _ discovered guessed i) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed ++ "\n"
    ++ "Wrong guesses so far: " ++ show i

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s l [] 0
    where l = fmap (const Nothing) s

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _ _) c = elem c s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ s _) c = elem c s

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s i) c =
  Puzzle word newFilledInSoFar (c:s) i
  where newFilledInSoFar = zipWith (zipper c) word filledInSoFar
        zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
 
handleWrongGuess :: Puzzle -> Puzzle
handleWrongGuess (Puzzle w f g i) =
  Puzzle w f g (i+1)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word\
               \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return $ handleWrongGuess (fillInCharacter puzzle guess)

gameOver :: Puzzle -> Int -> IO ()
gameOver (Puzzle wordToGuess _ _ i) maxWrong =
  if i >= maxWrong then
    do putStrLn "You lose"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle maxWrongGuesses
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"


main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
