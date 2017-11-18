{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module NumberOrString where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import Data.Text (Text)
import Text.RawString.QQ
import Data.Scientific (floatingOrInteger)

data NumberOrString = Numba Integer| Stringy Text deriving (Eq, Show)
instance FromJSON NumberOrString where
    parseJSON (Number i) =
        case floatingOrInteger i of
            (Left _) -> fail "Must be integral number"
            (Right integer) -> return $ Numba integer
    parseJSON (String s) = return $ Stringy s
    parseJSON _ = fail "NumberOrString must be number or string"

dec :: ByteString -> Maybe NumberOrString
dec = decode

eitherDec :: ByteString -> Either String NumberOrString
eitherDec = eitherDecode

main = do
    print $ dec "blah"
    print $ eitherDec "blah"