{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import System.IO
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Data.Text (Text)
import Data.String (fromString)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Options.Applicative
import Data.Semigroup ((<>))

type Host = String
type Port = String

data Config = Config { configHost :: Host
                     , configPort :: Port
                     } deriving (Eq, Show)

type ConfigT a = ReaderT Config IO a

sendData :: C.ByteString -> ConfigT ()
sendData byteString = do
    config <- ask
    liftIO $ withSocketsDo $ do
      addr <- resolve (configHost config) (configPort config)
      E.bracket (open addr) close (talk byteString)
  where
    resolve h p = do
      let hints = defaultHints { addrSocketType = Stream }
      addr:_ <- getAddrInfo (Just hints) (Just h) (Just p)
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) defaultProtocol --(addrProtocol addr)
      connect sock $ addrAddress addr
      return sock
    talk bs sock = do
      sendAll sock $ bs
      msg <- recv sock 1024
      C.putStrLn msg

sendJSON :: FilePath -> ConfigT ()
sendJSON filePath =
  (liftIO $ readFile filePath) >>= (sendData . fromString . (++ "\0"))

sendFinger :: String -> ConfigT ()
sendFinger = sendData . fromString . (++ "\r\n")

data Arguments = Finger String Config
               | FileInput FilePath Config
               deriving (Show, Eq)

configInput :: Parser Config
configInput = Config
  <$> strOption
    (  long "host"
    <> short 'h'
    <> metavar "HOST"
    <> help "host to connect to")
  <*> strOption
    (  long "port"
    <> short 'p'
    <> metavar "PORT"
    <> help "port of daemon")

fingerInput :: Parser Arguments
fingerInput = Finger
  <$> strOption
    (  long "name"
    <> short 'n'
    <> metavar "NAME"
    <> help "name of person to look up")
  <*> configInput

fileInput :: Parser Arguments
fileInput = FileInput
  <$> strOption
    (  long "file"
    <> short 'f'
    <> metavar "FILENAME"
    <> help "json file with config")
  <*> configInput

input :: Parser Arguments
input = fingerInput <|> fileInput

handleArguments :: Arguments -> IO ()
handleArguments (Finger n c) = runReaderT (sendFinger n) c
handleArguments (FileInput f c) = runReaderT (sendJSON f) c

main :: IO ()
main = handleArguments =<< execParser opts
  where
    opts = info (input <**> helper)
      ( fullDesc
     <> progDesc "Finger a person or update the server's database."
     <> header "client - a tool to talk to the fingerd" )

