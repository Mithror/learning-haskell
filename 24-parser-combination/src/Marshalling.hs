{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Marshalling where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import Data.Text (Text)
import Text.RawString.QQ

sectionJson :: ByteString
sectionJson = [r|
{ "section": {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"}
}
|]

data TestData = TestData { section :: Host, what :: Color } deriving (Eq, Show)

instance FromJSON TestData where
    parseJSON (Object v) =
        TestData <$> v .: "section"
                 <*> v .: "whatisit"
    parseJSON _ = fail "Expected an object for TestData"

newtype Host = Host String deriving (Eq, Show)

instance FromJSON Host where
    parseJSON (Object v) = 
        Host <$> v .: "host"
    parseJSON _ = fail "Expected an object for Host"

type Annotation = String

data Color = Red Annotation
           | Blue Annotation
           | Yellow Annotation
           deriving (Eq, Show)

instance FromJSON Color where
    parseJSON (Object v) =
        (Red <$> v .: "red") <|>
        (Blue <$> v .: "blue") <|>
        (Yellow <$> v .: "yellow")
    parseJSON _ = fail "Expected an object for Color"

main :: IO ()
main = do
    let d :: Maybe TestData
        d = decode sectionJson
    print d