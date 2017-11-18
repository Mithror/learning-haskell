{-# LANGUAGE OverloadedStrings #-}
module Text.Fractions where

import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta

badFraction :: IsString a => a
badFraction = "1/0"
alsoBad :: IsString a => a
alsoBad = "10"
shouldWork :: IsString a => a
shouldWork = "1/2"
shouldAlsoWork :: IsString a => a
shouldAlsoWork = "2/1"

-- parseFraction :: Parser Rational
-- parseFraction = do
--     numerator <- decimal
--     char '/'
--     denominator <- decimal
--     return (numerator % denominator)

-- virtuousFraction :: Parser Rational
-- virtuousFraction = do
--     numerator <- decimal
--     char '/'
--     denominator <- decimal
--     case denominator of
--         0 -> fail "Denominator cannot be zero"
--         _ -> return (numerator % denominator)

parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

main :: IO ()
main = do
    -- parseOnly is Attoparsec
    let attoP = parseOnly parseFraction
    print $ attoP badFraction
    print $ attoP shouldWork
    print $ attoP shouldAlsoWork
    print $ attoP alsoBad

    -- parseString is Trifecta
    let p = parseString parseFraction mempty
    print $ p badFraction
    print $ p shouldWork
    print $ p shouldAlsoWork
    print $ p alsoBad
    -- let p = parseString parseFraction mempty
    -- print $ p shouldWork
    -- print $ p shouldAlsoWork
    -- print $ p alsoBad
    -- print $ p badFraction

-- testVirtuous :: IO ()
-- testVirtuous = do
--     let virtuousFraction' = parseString virtuousFraction mempty
--     print $ virtuousFraction' shouldWork
--     print $ virtuousFraction' shouldAlsoWork
--     print $ virtuousFraction' alsoBad
--     print $ virtuousFraction' badFraction