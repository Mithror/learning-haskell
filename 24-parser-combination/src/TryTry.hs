module TryTry where

import Control.Applicative ((<|>))
import Text.Trifecta
import Data.Ratio ((%))

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

type DecimalOrFraction = Either Integer Rational

parseDecimalOrFraction :: Parser DecimalOrFraction
parseDecimalOrFraction = (Right <$> (try parseFraction)) 
                         <|> (Left <$> decimal)



