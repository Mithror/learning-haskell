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

saveURI' :: BC.ByteString
         -> BC.ByteString
         -> ReaderT R.Connection IO (Either R.Reply R.Status)
saveURI' shortURI uri = do
  conn <- ask
  -- conn :: R.Connection
  -- R.set shortURI uri :: R.RedisCtx m f => m (f R.Status)
  -- R.runRedis conn $ R.set shortURI uri :: IO (Either R.Reply R.Status)
  -- either liftIO or lift should work:
  -- liftIO $ R.runRedis conn $ R.set shortURI uri
  lift $ R.runRedis conn $ R.set shortURI uri

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
    -- get "/" :: ActionM () -> ScottyM ()
    -- We need this to be a ReaderT R.Connection ScottyM ()
    -- mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
    -- Just what we need!
    mapReaderT (get "/") $ do
      -- We are in the ReaderT R.Connection ActionM () monad
      -- param "uri" :: Parsable a => ActionM a
      -- which we lift into the ReaderT R.Connection
      -- lift :: m a -> t m a (t here is ReaderT R.Connection)
      uri <- lift $ param "uri"
      let parsedUri :: Maybe URI
          parsedUri = parseURI (TL.unpack uri)
      case parsedUri of
        Just _  -> do
          -- shortyGen :: IO String
          -- needs to be made into ReaderT R.Connection ActionM a
          -- liftIO :: MonadIO m => IO a -> m a
          -- ReaderT R.Connection ActionM is an MonadIO, so we're good
          shawty <- liftIO shortyGen
          let shorty = BC.pack shawty
              uri' = encodeUtf8 (TL.toStrict uri)
          -- mySave shawty :: IO (Either R.reply R.status) -> ActionM ()
          --we need to map that into the ReaderT using mapReaderT again
          -- which gives us a
          -- ReaderT R.Connection IO (Either R.Reply R.Status) ->
          --   ReaderT R.Connection ActionM ()
          mapReaderT (mySave shawty) $ saveURI' shorty uri'
          -- text (shortyAintUri uri) :: ActionM () , so we lift this into
          -- ReaderT R.Connection
        Nothing -> lift $ text (shortyAintUri uri)
    mapReaderT (get "/:short") $ do
      short <- lift $ param "short"
      mapReaderT myGet $ getURI' short

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 $ runReaderT app rConn
