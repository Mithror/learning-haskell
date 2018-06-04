module DOTParser where

import Text.Trifecta
import Data.Char (toLower, toUpper)
import Control.Applicative ((<|>))

iChar :: Char -> Parser Char
iChar c = char (toLower c) <|> char (toUpper c)

iString :: String -> Parser String
iString s = sequenceA $ fmap iChar s

data CompassPT = N | NE | E | SE | S | SW | W | NW | C | O deriving (Show, Eq)

pCompass :: Parser CompassPT
pCompass =
    -- North
    try (iString "ne" *> pure NE) <|>
    try (iString "nw" *> pure NW) <|>
    (iString "n" *> pure N) <|>
    -- East
    (iString "e" *> pure E) <|>
    -- South
    try (iString "se" *> pure SE) <|>
    try (iString "sw" *> pure SW) <|>
    (iString "s" *> pure S) <|>
    -- West
    (iString "w" *> pure W) <|>
    -- Others
    (iString "c" *> pure C) <|> (string "_" *> pure O)

newtype ID = ID String deriving (Eq, Show)

