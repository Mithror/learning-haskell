{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
-- import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

import Control.Monad.IO.Class

data Config =
  Config {
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = let m' = M.insertWith (+) k 1 m
                in (m', (M.!) m' k)

updateHitCounter :: Text -> ReaderT Config IO Integer
updateHitCounter t = do
  c <- ask
  m <- liftIO $ readIORef (counts c)
  let (m', n) = bumpBoomp t m
  liftIO $ writeIORef (counts c) m'
  return n

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    config <- lift ask
    let key' = mappend (prefix config) unprefixed
    newInteger <- lift $ updateHitCounter key'
    html $
      mconcat [ "<h1>Success! Count was: "
              , TL.pack $ show newInteger
              , "</h1>"
              ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config { counts = counter, prefix = TL.pack prefixArg }
      runR = flip runReaderT config
  scottyT 3000 runR app
