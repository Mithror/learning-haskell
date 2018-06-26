{-# LANGUAGE OverloadedStrings #-}

module Main where

import UserDB

import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getArgs)
import System.IO (hSetBuffering, BufferMode(..), stdout)
import Data.String (fromString)

addUser' :: Text -> IO ()
addUser' name = do
    s <- fmap fromString $ putStr "Shell: " >> getLine
    h <- fmap fromString $ putStr "Home Dir: " >> getLine
    r <- fmap fromString $ putStr "Real name: " >> getLine
    p <- fmap fromString $ putStr "Phone no.: " >> getLine
    b <- addUser $ User 0 name s h r p
    case b of
        True -> putStrLn "User succesfully added."
        False -> putStrLn "User already exists"

updateUser' :: Text -> IO ()
updateUser' name = do
    let f "" = Nothing
        f a  = Just a
    putStrLn "Use blank if you don't want to update."
    s <- fmap f $ putStr "Shell: " >> getLine
    h <- fmap f $ putStr "Home Dir: " >> getLine
    r <- fmap f $ putStr "Real name: " >> getLine
    p <- fmap f $ putStr "Phone no.: " >> getLine
    b <- updateUser (T.unpack name) s h r p
    case b of
        True -> putStrLn "User succesfully updated."
        False -> putStrLn "No such user found."

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case args of
        ["add", name] -> addUser' $ T.pack name
        ["update", name] -> updateUser' $ T.pack name
        _ -> putStrLn "add <name> | update <name>"