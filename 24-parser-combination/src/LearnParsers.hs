module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators (eof)

import Control.Applicative ((<|>))

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

testParse :: Show a => Parser a -> IO ()
testParse p = print $ parseString p mempty "123"

-- testEOFParse :: Parser () -> IO ()
-- testEOFParse p = print $ parseString p mempty "123"

pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

-- 1
one'' :: Parser ()
one'' = one >> eof

oneTwo'' :: Parser ()
oneTwo'' = oneTwo >> eof

-- 2
-- Looking at the documentation you can combine strings with <|>
oneTwoThree :: Parser String
-- Put the biggest string first as it matches from from left to right
oneTwoThree = string "123" <|> string "12" <|> string "1"

-- Other solution is to use choice to create a parser from other parser
-- choice :: [Parser a] -> Parser a
oneTwoThree' :: Parser String
oneTwoThree' = choice [string "1", string "12", string "123"]

-- 3
-- Using foldr:
string' :: String -> Parser String
string' = foldr (\c p -> (:) c <$> p) (pure [])

-- We haven't used the `char` function, if we do want to use it, we have
-- to use <*>:
string'' :: String -> Parser String
string'' = foldr (\c p -> (:) <$> char c <*> p) (pure [])

-- Which is basically:
string''' :: String -> Parser String
string''' = traverse char



main :: IO ()
main = do
    pNL "stop:"
    testParse (stop :: Parser Char)
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo'"
    testParse oneTwo'
    pNL "one'':"
    testParse one''
    pNL "oneTwo'':"
    testParse oneTwo''
