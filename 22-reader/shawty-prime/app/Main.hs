{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty

import Control.Monad.Reader (ReaderT(..), runReaderT, ask, lift, mapReaderT)

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  -- Right of arrow is IO Int, so randomDigit is Int
  randomDigit <- SR.randomRIO (0, maxIndex) :: IO Int
  return (xs !! randomDigit)

shortyGen :: IO String
shortyGen =
  replicateM 7 (randomElement alphaNum)

saveURI :: R.Connection
        -> BC.ByteString
        -> BC.ByteString
        -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.set shortURI uri

-- No idea how to use this now...
saveURI' :: BC.ByteString 
        -> BC.ByteString 
        -> ReaderT R.Connection IO (Either R.Reply R.Status)
saveURI' shortURI uri = do
  conn <- ask
  liftIO $ R.runRedis conn $ R.set shortURI uri

getURI  :: R.Connection
        -> BC.ByteString
        -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI = R.runRedis conn $ R.get shortURI

-- No idea how to use this now...
getURI' :: BC.ByteString
       -> ReaderT R.Connection IO (Either R.Reply (Maybe BC.ByteString))
getURI' shortURI = do
  conn <- ask
  liftIO $ R.runRedis conn $ R.get shortURI

linkShorty :: String -> String
linkShorty shorty =
  concat [ "<a href=\""
         , shorty
         , "\">Copy and paste your short URL</a>"
         ]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat [ TL.pack (show resp)
            , " shorty is: ", TL.pack (linkShorty shawty)
            ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat [ uri
            , " wasn't a url, did you forget http://?"
            ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

mySave :: String -> IO (Either R.Reply R.Status) -> ActionM ()
mySave shawty respIO = do
  resp <- lift respIO
  html (shortyCreated resp shawty)

myGet :: IO (Either R.Reply (Maybe BC.ByteString)) -> ActionM ()
myGet respIO = do
  uri <- lift respIO
  case uri of
    Left reply -> text (TL.pack (show reply))
    Right mbBS -> case mbBS of
      Nothing -> text "uri not found"
      Just bs -> html (shortyFound tbs)
        where tbs :: TL.Text
              tbs = TL.fromStrict (decodeUtf8 bs)

app :: ReaderT R.Connection ScottyM ()
app = do
    mapReaderT (get "/") $ do
      uri <- lift $ param "uri"
      let parsedUri :: Maybe URI
          parsedUri = parseURI (TL.unpack uri)
      case parsedUri of
        Just _  -> do
          shawty <- liftIO shortyGen
          let shorty = BC.pack shawty
              uri' = encodeUtf8 (TL.toStrict uri)
          mapReaderT (mySave shawty) $ saveURI' shorty uri'
        Nothing -> lift $ text (shortyAintUri uri)
    mapReaderT (get "/:short") $ do
      short <- lift $ param "short"
      mapReaderT myGet $ getURI' short

-- app :: ReaderT R.Connection ScottyM ()
-- app =  do
--   rConn <- ask
--   lift $ do
--     get "/" $ do
--       uri <- param "uri"
--       let parsedUri :: Maybe URI
--           parsedUri = parseURI (TL.unpack uri)
--       case parsedUri of
--         Just _  -> do
--           shawty <- liftIO shortyGen
--           let shorty = BC.pack shawty
--               uri' = encodeUtf8 (TL.toStrict uri)
--           resp <- liftIO (saveURI rConn shorty uri')
--           html (shortyCreated resp shawty)
--         Nothing -> text (shortyAintUri uri)
--     get "/:short" $ do
--       short <- param "short"
--       uri <- liftIO (getURI rConn short)
--       case uri of
--         Left reply -> text (TL.pack (show reply))
--         Right mbBS -> case mbBS of
--           Nothing -> text "uri not found"
--           Just bs -> html (shortyFound tbs)
--             where tbs :: TL.Text
--                   tbs = TL.fromStrict (decodeUtf8 bs)

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 $ runReaderT app rConn 
-- main = do
--   rConn <- R.connect R.defaultConnectInfo
--   scotty 3000 (app rConn)

