module Morra
    (runGame, Game(..), Player(..), PlayerType(..))
    where

import Data.Maybe (listToMaybe)
import qualified Data.Map as M

import Control.Monad (forever)
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.Console.ANSI
import System.IO (hFlush, stdout)

-- Two player game
-- Player chooses 0-5 and number
-- AI chooses 0-5 and number

type Source = (Integer, Integer)
type Table = M.Map Source Integer
type Guesses = (Integer, Integer)

data PlayerType = Human | AI Guesses Table deriving (Show, Eq)

data Player = Player {
    pType :: PlayerType,
    score :: Integer
} deriving (Show, Eq)

data Game = Game {
    playerA :: Player,
    playerB :: Player,
    pointsToWin :: Integer
} deriving (Show, Eq)

-- type Score = (Integer, Integer)

type GameState = StateT Game IO

isPlayerWinner :: (Game -> Player) -> Game -> Bool
isPlayerWinner p g = (score . p) g >= pointsToWin g

endGameMsg :: Bool -> Bool -> IO ()
endGameMsg True True = putStrLn "Everybody is a winner!" >> exitSuccess
endGameMsg True False = putStrLn "Player A is a winner!" >> exitSuccess
endGameMsg False True = putStrLn "Player B is a winner!" >> exitSuccess
endGameMsg _ _ = return ()

gameEnd :: GameState ()
gameEnd = do
    g <- get
    let pA = isPlayerWinner playerA g
        pB = isPlayerWinner playerB g
    liftIO $ endGameMsg pA pB
    return ()

handleHumanPlayer :: IO (Integer, Integer)
handleHumanPlayer = do
    putStr "Fingers and total (f, t): "
    hFlush stdout
    s <- getLine
    clearScreen
    case fmap fst . listToMaybe . reads $ s of
        Nothing -> do
            putStrLn "Incorrect format."
            handleHumanPlayer
        Just a -> return a

handleAIPlayer :: Guesses -> Table -> IO (Integer, Integer)
handleAIPlayer g t = do
    f <- randomRIO (0,5)
    g <- case M.lookup g t of
        Nothing -> randomRIO (0, 5)
        Just n -> return n
    return (f, f + g)

updateAI :: Player -> Integer -> Player
updateAI p n = case pType p of
    Human -> p
    AI g@(g1, g2) t -> p { pType = AI g' t' }
        where g' = (g2, n)
              t' = M.insert g n t

updateAIs :: (Integer, Integer) -> GameState ()
updateAIs (gA, gB) = do
    g <- get
    let pA = playerA g
        pB = playerB g
        pA' = updateAI pA gB
        pB' = updateAI pB gA
        g' = g { playerA = pA' }
    put $ g' { playerB = pB' }

handlePlayer :: (Game -> Player) -> GameState (Integer, Integer)
handlePlayer p = do
    g <- get
    let player = p g
    case pType player of
        Human -> liftIO handleHumanPlayer
        AI g t -> liftIO $ handleAIPlayer g t

handleGuessPlayerA :: Integer -> Integer -> GameState ()
handleGuessPlayerA guess total =
    if (guess == total)
    then do
        g <- get
        let p = playerA g
            s = score p
        put $ g { playerA = (p { score = s + 1 })}
        liftIO $ putStrLn $ "PlayerA guessed correctly!"
    else return ()

handleGuessPlayerB :: Integer -> Integer -> GameState ()
handleGuessPlayerB guess total =
    if (guess == total)
    then do
        g <- get
        let p = playerB g
            s = score p
        put $ g { playerB = (p { score = s + 1 })}
        liftIO $ putStrLn $ "PlayerB guessed correctly!"
    else return ()

runGame :: GameState ()
runGame = forever $ do
    gameEnd
    liftIO $ putStrLn "Turn PlayerA"
    (f, g) <- handlePlayer playerA
    liftIO $ putStrLn "Turn PlayerB"
    (f', g') <- handlePlayer playerB
    let total = f + f'
    liftIO $ do
        putStrLn "=========================="
        putStr $ "PlayerA showed: " ++ show f
        putStrLn $ " and guessed: " ++ show g
        putStr $ "PlayerB showed: " ++ show f'
        putStrLn $ " and guessed: " ++ show g'
        putStrLn $ "Total was: " ++ show total
        putStrLn "=========================="
    handleGuessPlayerA g total
    handleGuessPlayerB g' total
    liftIO $ putStrLn "=========================="
    -- update any ai
    updateAIs (f, f')
    -- print the player status
    g <- get
    let pA = playerA g
        pB = playerB g
    liftIO $ do
        putStrLn $ "PlayerA: " ++ show pA
        putStrLn $ "PlayerB: " ++ show pB