{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Text.Trifecta
import System.Directory (listDirectory)
import System.FilePath (takeExtension)
import Control.Exception (try)
import System.Environment (getArgs)

newtype Header = Header String deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

assignmentEx :: ByteString
assignmentEx = "woot=1"

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
    name <- some letter
    _ <- char '='
    val <- some (noneOf "\n")
    skipEOL -- important!
    return (name, val)

    -- | Skip end of lien and whitespace beyond.
skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

-- | Skip comments starting at the beginning of the line
skipComments :: Parser ()
skipComments = skipMany $ do
    _ <- char ';' <|> char '#'
    skipMany (noneOf "\n")
    skipEOL

data Section = Section Header Assignments deriving (Eq, Show)

newtype Config = Config (Map Header Assignments) deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
    skipWhitespace
    skipComments
    h <- parseHeader
    skipEOL
    assignments <- some parseAssignment
    return $ Section h (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
    sections <- some parseSection
    let mapOfSections = foldr rollup M.empty sections
    return $ Config mapOfSections

parseIniFromFile :: FilePath -> IO (Maybe Config)
parseIniFromFile fp = do
    s <- readFile fp
    let parsed = parseString parseIni mempty s
    case parsed of
        (Success a) -> return $ Just a
        _ -> return Nothing

main :: IO ()
main = do
    args <- getArgs
    case args of
        [dir] -> do
            l <- Control.Exception.try $ listDirectory dir
            case l of
                Left e -> ioError e
                Right xs -> do
                    let xs' = filter ((==) ".ini" . takeExtension) xs
                        f x = sequenceA (x, parseIniFromFile (dir ++ "/" ++ x))
                        xs'' = sequenceA $ fmap f xs'
                    xs''' <- xs''
                    print $ M.fromList xs'''
        _ -> putStrLn "Need directory as argument."