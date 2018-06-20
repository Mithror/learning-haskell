{-# LANGUAGE OverloadedStrings #-}

module Main where

import UserDB (User(..), getAllUsers, getSingleUser, addUser)

import Control.Concurrent
-- import Control.Concurrent.MVar
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Socket hiding ({-close,-} recv)
import Network.Socket.ByteString (recv, sendAll)
import Data.Aeson

getAllUsers' :: MVar Bool -> IO [User]
getAllUsers' m = do
  m' <- takeMVar m
  users <- getAllUsers
  putMVar m m'
  return users

addUser' :: MVar Bool -> User -> IO ()
addUser' m u = do
  m' <- takeMVar m
  b <- addUser u
  print b
  putMVar m m'

formatUser :: User -> ByteString
formatUser (User _ uname sh homeDir rName _) = BS.concat
    [ "Login: ", e uname, "\t\t\t\t"
    , "Name: ", e rName, "\n"
    , "Directory: ", e homeDir, "\t\t\t"
    , "Shell: ", e sh, "\n"]
    where e = encodeUtf8

returnUsers :: Socket -> MVar Bool -> IO ()
returnUsers soc m = do
  rows <- getAllUsers-- m
  let usernames = map username rows
      newlineSeparated = T.concat $ intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

returnUser :: Socket -> Text -> IO ()
returnUser soc uname = do
  maybeUser <- getSingleUser uname
  case maybeUser of
    Nothing -> do
      putStrLn $ "Couldn't find matching user for username: " ++ show uname
      return ()
    Just user -> sendAll soc (formatUser user)

handleQuery :: Socket -> MVar Bool -> IO ()
handleQuery soc m = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers soc m
    name -> returnUser soc (decodeUtf8 name)

handleQueries :: Socket -> MVar Bool -> IO ()
handleQueries sock m = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery soc m
  Network.Socket.close soc -- sClose soc is deprecated

fingerd :: MVar Bool -> IO ()
fingerd m = do
  withSocketsDo $ do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                             Nothing
                             (Just "79")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    Network.Socket.bind sock (addrAddress serveraddr)
      -- bindSocket sock (addrAddress serveraddr) is deprecated
    listen sock 1
    -- only one connection open at a time
    handleQueries sock m
    Network.Socket.close sock -- sClose sock is deprecated

recvJson :: Socket -> IO ByteString
recvJson soc = do
  msg <- recv soc 1024
  let l = BS.length msg
  case l of
    0 -> do
      msg' <- recvJson soc
      return $ msg `BS.append` msg'
    _ -> case BS.last msg of
      0 -> return $ BS.take (l - 1) msg
      _ -> do
        msg' <- recvJson soc
        return $ msg `BS.append` msg'
  -- return msg

handleRequest :: Socket -> MVar Bool -> IO ()
handleRequest sock m = forever $ do
  (soc, _) <- accept sock
  putStrLn "Woohoo!"
  j <- recvJson soc
  print j
  case decode (BSL.fromStrict j) of
    Nothing -> putStrLn "Cannot decode."
    (Just u) -> do
      putStrLn $ "Decoded User: " ++ show u
      addUser' m u
  Network.Socket.close soc -- sClose soc is deprecated

add :: MVar Bool -> IO ()
add m = do
  withSocketsDo $ do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                             Nothing
                             (Just "4242")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    Network.Socket.bind sock (addrAddress serveraddr)
      -- bindSocket sock (addrAddress serveraddr) is deprecated
    listen sock 1
    -- only one connection open at a time
    handleRequest sock m
    Network.Socket.close sock -- sClose sock is deprecated

main :: IO ()
-- main = createDatabase
main = do
  m <- newMVar True
  _ <- forkIO $ add m
  fingerd m