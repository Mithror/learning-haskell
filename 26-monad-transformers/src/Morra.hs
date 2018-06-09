module Morra where

import Control.Monad (forever)
import Control.Monad.Trans.State
import Control.Monad.IO.Class


import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- Player chooses 0-5 and number
-- AI chooses 0-5 and number

pointsToWin :: Integer
pointsToWin = 3

type Score = (Integer, Integer)
type GameState = StateT Score IO

iWin :: Score -> Bool
iWin = (>= pointsToWin) . fst

aiWin :: Score -> Bool
aiWin = (>= pointsToWin) . snd

gameWin :: GameState ()
gameWin = do
    s <- get
    if iWin s then
        liftIO $ do
            putStrLn "You win!"
            exitSuccess
    else return ()

gameLose :: GameState ()
gameLose = do
    s <- get
    if aiWin s then
        liftIO $ do
            putStrLn "AI wins!"
            exitSuccess
    else return ()

getFingers :: IO Integer
getFingers = do
    putStrLn "How many fingers will you show (0-5)?"
    response <- getLine
    if response `elem` (map show [0..5])
        then return $ read response
        else getFingers

getGuess :: IO Integer
getGuess = do
    putStrLn "How many fingers will be shown in total (0-10)?"
    response <- getLine
    if response `elem` (map show [0..10])
        then return $ read response
        else getGuess

getAIData :: IO (Integer, Integer)
getAIData = do
    f <- randomRIO (0,5)
    g <- randomRIO (0,5)
    return (f, f + g)

handleCorrectGuess :: GameState ()
handleCorrectGuess = do
    (hs,as) <- get
    put (hs + 1, as)

handleAICorrectGuess :: GameState ()
handleAICorrectGuess = do
    (hs, as) <- get
    put (hs, as + 1)

runGame :: GameState ()
runGame = forever $ do
    gameWin
    gameLose
    (f,g) <- liftIO $ do
            f <- getFingers
            g <- getGuess
            return (f,g)
    (f', g') <- liftIO getAIData
    if f == g + g' then handleCorrectGuess else return ()
    if f' == g + g' then handleAICorrectGuess else return ()
    s <- get
    liftIO $ do
        putStrLn $ "=================="
        putStrLn $ "AI showed: " ++ show f'
        putStrLn $ "AI guessed: " ++ show g'
        putStrLn $ "Total was: " ++  show (f + f')
        putStrLn $ "------------------"
        putStrLn $ "Score is: " ++ show s
        putStrLn $ "=================="


initialState :: Score
initialState = (0, 0)

main :: IO ()
main = evalStateT runGame initialState