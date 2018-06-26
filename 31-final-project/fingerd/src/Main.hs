{-# LANGUAGE OverloadedStrings #-}

module Main where

import UserDB (User(..), getAllUsers, getSingleUser, addUser,
               updateUser, deleteUser)

import Control.Concurrent
import Control.Monad (forever)
import Data.List (intersperse, intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Data.Aeson
import Control.Applicative (empty)
import qualified Data.HashMap.Lazy as HML

import Data.Aeson.Types
import Data.Either (lefts)
import Data.String (fromString)

-- type UserName = Text

-- {
--   "actions" : [
--     {
--       "action" : "delete",
--       "params" : "gdp"
--     },
--     {
--       "action" : "add",
--       "params" : {
--         "username" : "gdp",
--         "shell" : "/bin/sh",
--         "homeDirectory" : "/home/gdp",
--         "realName" : "Gaël Depreeuw",
--         "phone" : "555-123-456789"
--       }
--     },
--     {
--       "action" : "update",
--       "params" : {
--         "username" : "gdp",
--         "newUsername" : "...",
--         "newShell" : "...",
--         "newHomedirectory" : "...",
--         "newRealName" : "...",
--         "newPhone" : "...",
--      }
--     }
--   ]
-- }

data UserUpdate = UserUpdate {
  name :: String,
  mbUsername :: Maybe String,
  mbShell :: Maybe String,
  mbHomeDirectory :: Maybe String,
  mbRealName :: Maybe String,
  mbPhone :: Maybe String
} deriving (Show, Eq)
instance FromJSON UserUpdate where
  parseJSON = withObject "UserUpdate" $ \o -> UserUpdate
    <$> o .: "username"
    <*> o .:? "newUsername"
    <*> o .:? "shell"
    <*> o .:? "homeDirectory"
    <*> o .:? "realName"
    <*> o .:? "phone"

data Action = DeleteAction Text
            | AddUser User
            | ModifyUser UserUpdate
            deriving (Show, Eq)

instance FromJSON Action where
  parseJSON = withObject "Action" $ \o -> do
    a <- o .: "action" :: Parser Text
    let mbValue = HML.lookup "params" o
    case mbValue of
      Nothing -> empty
      Just v -> case a of
                  "delete" -> DeleteAction <$> o .: "params"
                  "add" -> AddUser <$> parseJSON v
                  "update" -> ModifyUser <$> parseJSON v
                  _ -> empty

data Config = Config [Action] deriving (Show, Eq)
instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> Config <$> o .: "actions"

-- instance FromJSON Config where
--   parseJSON = withObject "Config" $ \o ->
--     case HML.lookup "actions" o of
--       (Just (Array v)) -> Config <$> (mapM parseJSON $ V.toList v)
--       _ -> empty

-- (.:) :: FromJSON a => Object -> Text -> Parser a
-- parseJSON :: FromJSON a => Value -> Parser a
-- withObject :: String -> (Object -> Parser a) -> Value -> Parser a
-- withArray :: String -> (Array -> Parser a) -> Value -> Parser a

doAction :: MVar () -> Action -> IO (Either String ())
doAction m (DeleteAction t) = do
  b <- protect m $ deleteUser t
  case b of
    True -> return $ Right ()
    False -> return $ Left ("User " ++ (T.unpack t)
                            ++ " does not exist.")
doAction m (AddUser u) = do
  b <- protect m $ addUser u
  case b of
    True -> return $ Right ()
    False -> return $ Left ("User " ++ (T.unpack $ username u)
                                   ++ " exists already.")
doAction m (ModifyUser (UserUpdate n _ s h r p)) = do
  b <- protect m $ updateUser n s h r p
  case b of
    True -> return $ Right ()
    False -> return $ Left ("User " ++ n
                                   ++ " does not exist.")

doConfig :: MVar () -> Config -> IO (Either String ())
doConfig m (Config xs) =
  f . lefts <$>  mapM (doAction m) xs
  where f [] = Right ()
        f ys = Left $ intercalate "\n" ys

-- creates a critical section of the IO action
protect :: MVar () -> IO a -> IO a
protect m act = do
  takeMVar m
  a <- act
  putMVar m ()
  return a

formatUser :: User -> ByteString
formatUser (User _ uname sh homeDir rName _) = BS.concat
    [ "Login: ", e uname, "\t\t\t\t"
    , "Name: ", e rName, "\n"
    , "Directory: ", e homeDir, "\t\t\t"
    , "Shell: ", e sh, "\n"]
    where e = encodeUtf8

returnUsers :: Socket -> MVar () -> IO ()
returnUsers soc m = do
  rows <- protect m getAllUsers
  let usernames = map username rows
      newlineSeparated = T.concat $ (intersperse "\n" usernames)
  sendAll soc (encodeUtf8 $ T.append newlineSeparated "\n")

returnUser :: Socket -> MVar () -> Text -> IO ()
returnUser soc m uname = do
  maybeUser <- protect m $ getSingleUser uname
  case maybeUser of
    Nothing -> do
      putStrLn $ "Couldn't find matching user for username: " ++ show uname
      return ()
    Just user -> sendAll soc (formatUser user)

-- Modified this to accept message longer than 1024
-- keeping in mind that \r\n might be split over two packages
recvCommand :: Socket -> IO (Maybe ByteString)
recvCommand soc = getData >>= checkForCRLF
  where
    getData = recv soc 1024
    checkForCRLF b =
      if BS.isInfixOf "\r\n" b
         then return . Just . BS.init . head . BS.split 0x0A $ b
         else getData >>= stopOrContinue b
    stopOrContinue b msg =
      if BS.null b
         then return Nothing
         else checkForCRLF $ BS.append b msg

handleQuery :: Socket -> MVar () -> IO ()
handleQuery soc m = do
  msg <- recvCommand soc
  case msg of
    Nothing -> putStrLn "Client closed socket."
    Just "" -> returnUsers soc m
    Just n -> returnUser soc m (decodeUtf8 n)

handleQueries :: Socket -> MVar () -> IO ()
handleQueries sock m = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery soc m
  close soc

fingerd :: MVar () -> IO ()
fingerd m = do
  withSocketsDo $ do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                             Nothing
                             (Just "79")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bind sock (addrAddress serveraddr)
    -- only one connection open at a time
    listen sock 1
    handleQueries sock m
    close sock

recvJson :: Socket -> IO (Maybe ByteString)
recvJson soc = recv soc 1024 >>= stopOrContinue
  where stopOrContinue b
          | BS.null b = return Nothing
          | BS.last b == 0 = (return . Just $ BS.init b)
          | otherwise = (fmap . fmap) (BS.append b) (recvJson soc)

handleRequest :: Socket -> MVar () -> IO ()
handleRequest sock m = forever $ do
  (soc, _) <- accept sock
  j <- recvJson soc
  resp <- case j >>= (decode . BSL.fromStrict) of
    Nothing -> return "Cannot decode json.\r\n"
    (Just c) -> createMsg <$> doConfig m c
  sendAll soc resp
  close soc
  where createMsg (Right _) = "Actions succesfully applied.\r\n"
        createMsg (Left s) = fromString s

add :: MVar () -> IO ()
add m = do
  withSocketsDo $ do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                             Nothing
                             (Just "4242")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bind sock (addrAddress serveraddr)
    listen sock 1
    handleRequest sock m
    close sock

main :: IO ()
main = do
  m <- newMVar ()
  _ <- forkIO $ add m
  fingerd m