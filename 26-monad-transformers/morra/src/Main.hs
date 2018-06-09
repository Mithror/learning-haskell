module Main where

import Morra
import Control.Monad.Trans.State
import qualified Data.Map as M

initialGame :: Game
initialGame = Game {
  playerA = Player { pType = Human, score = 0 },
  playerB = Player { pType = AI (0,0) M.empty, score = 0 },
  pointsToWin = 3
}

main :: IO ()
main = do
  -- putStrLn "Woop"
  evalStateT runGame initialGame
