module FixTheCode where

import Control.Monad.Trans.Maybe
import Control.Monad

import Control.Monad.IO.Class

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
    v <- liftIO getLine -- added liftIO
    guard $ isValid v
    return v

doExcite :: IO ()
doExcite = do
    putStrLn "say something excite!"
    excite <- runMaybeT maybeExcite -- added runMaybeT
    case excite of
        Nothing -> putStrLn "MOAR EXCITE"
        Just e -> putStrLn $ "Good, was very excite: " ++ e
