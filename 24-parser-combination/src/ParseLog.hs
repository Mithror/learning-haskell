{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParseLog where

import Control.Applicative ((<|>), liftA2, liftA3)
import Data.Monoid ((<>))
import Text.Trifecta
import Data.Time.Format
import Data.Time
import Data.List (intersperse)
import Text.RawString.QQ
import Text.Parser.Token
import Control.Monad (replicateM)
import Data.Maybe
import Test.QuickCheck
import Data.Char

data LogEntry = LogEntry TimeOfDay String deriving Eq
instance Show LogEntry where
    show (LogEntry t s) =
        let time = formatTime defaultTimeLocale "%H:%M" t
        in time ++ " " ++ s
instance Arbitrary LogEntry where
    arbitrary = do
        tod <- liftA3 TimeOfDay
                      (choose (0,23))
                      (choose (0,59))
                      (pure 0)
        s <- liftA2 (++)
                    arbitrary
                    (oneof [((++) "--") <$> arbitrary, arbitrary])
        return $ LogEntry tod (filter (\c -> c /= '\n') s)

data DayEntry = DayEntry Day [LogEntry] deriving Eq
instance Show DayEntry where
    show (DayEntry d es) =
        let day = formatTime defaultTimeLocale "%Y-%m-%d" d
            entries = concat . intersperse "\n" . map show $ es
        in "# " ++ day ++ "\n" ++ entries
instance Arbitrary DayEntry where
    arbitrary = do
        day <- ModifiedJulianDay <$> (2000 +) <$> arbitrary
        es <- listOf arbitrary
        return $ DayEntry day es

data Log = Log [DayEntry] deriving Eq
instance Show Log where
    show (Log ds) = concat . intersperse "\n\n" . map show $ ds
instance Arbitrary Log where
    arbitrary = do
        es <- listOf arbitrary
        return $ Log es

countTimeSpentDay :: [LogEntry] -> DiffTime
countTimeSpentDay [] = 0
countTimeSpentDay (_:[]) = 0
countTimeSpentDay (x:y:xs) =
    let (LogEntry t1 _) = x
        (LogEntry t2 _) = y
    in   (timeOfDayToTime t2)
       - (timeOfDayToTime t1)
       + (countTimeSpentDay (y:xs))

countDay :: DayEntry -> DiffTime
countDay (DayEntry _ e) = countTimeSpentDay e

countTime :: Log -> DiffTime
countTime (Log ds) = sum $ map countDay ds

skipComment :: (Monad m, TokenParsing m) => m ()
skipComment = token $ do
    string "--"
    manyTill anyChar $ try newline
    return ()

myToken :: (Monad m, TokenParsing m) => m a -> m a
myToken p = token p <* (token $ skipSome skipComment <|> pure ())

-- token :: m a -> m a
-- token p = p <* (someSpace <|> pure ())

parseHour :: Parser String
parseHour = case1 <|> case2
    where case1 = sequenceA [oneOf "01", digit]
          case2 = sequenceA [char '2', oneOf "0123"]

parseMinutes :: Parser String
parseMinutes = sequenceA [ oneOf "012345", digit]

parseTimeOfDay :: Parser TimeOfDay
parseTimeOfDay = do
    timeString <- parseHour <> string ":" <> parseMinutes
    case parseTimeM True defaultTimeLocale "%H:%M" timeString of
        Just time -> return time
        Nothing -> unexpected "Incorrect time format"

parseLogEntry :: Parser LogEntry
parseLogEntry = myToken $ do
    tod <- parseTimeOfDay
    space
    s <- manyTill anyChar ((newline >> return ()) <|> skipComment)
    return $ LogEntry tod s

parseYear :: Parser String
parseYear = count 4 digit

-- This is actually also a cool way to do this, where the year
-- can consist up to 4 digits instead of 4, but this is not in the spirit of
-- the exercise I believe. year should be in "YYYY" format.
-- parseYear = liftA2 (:) digit (catMaybes <$> replicateM 3 d)
--     where d = try $ optional digit

parseMonth :: Parser String
parseMonth = case1 <|> case2
    where case1 = sequenceA [ char '0', digit ]
          case2 = sequenceA [ char '1', oneOf "012"]

parseDay :: Parser String
parseDay = case1 <|> case2
    where case1 = sequenceA [ oneOf "012", digit ]
          case2 = sequenceA [ char '3', oneOf "01" ]

parseDay' :: Parser Day
parseDay' = do
    day <- parseYear <> string "-" <> parseMonth <> string "-" <> parseDay
    case parseTimeM True defaultTimeLocale "%Y-%m-%d" day of
        Just day' -> return day'
        Nothing -> unexpected "Incorrect day format"

parseDayEntry :: Parser DayEntry
parseDayEntry = myToken $ do
    char '#'
    space
    day <- myToken $ parseDay'
    logEntries <- many parseLogEntry
    return $ DayEntry day logEntries

parseLog :: Parser Log
parseLog  = do
    whiteSpace
    skipMany skipComment
    dayEntries <- many parseDayEntry
    return $ Log dayEntries

testLog :: String
testLog = [r|-- wheee a comment


# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

-- Generator for Log files
genBlankLine :: Gen String
genBlankLine = oneof [return "\t -- comment!\n", return " \t\n"]
genDayLine :: Gen String
genDayLine = do
    day <- ModifiedJulianDay <$> (2000 +) <$> arbitrary
    let day_string = formatTime defaultTimeLocale "%Y-%m-%d" day
    oneof [ return $ "# " ++ day_string ++ "\t \n"
          , return $ "# " ++ day_string ++ "\t -- comment!\n" ]
genLogEntry :: Gen String
genLogEntry = do
    time <- TimeOfDay <$> choose (0,23) <*> choose (0,59) <*> pure 0
    let time_string = formatTime defaultTimeLocale "%H:%M" time
    oneof [ return $ time_string ++ " some description \t \n"
          , return $ time_string ++ " some description \t -- with comment\n"]
genDayEntry :: Gen String
genDayEntry = do
    day <- genDayLine
    logEntry <- genLogEntry -- at least one log entry
    logEntryList <- concat <$> listOf genLogEntry
    blankLines <- concat <$> listOf genBlankLine
    return $ day ++  logEntry ++ logEntryList ++ blankLines
genLog :: Gen String
genLog = do
    blankLines <- concat <$> listOf genBlankLine
    dayEntries <- concat <$> listOf genDayEntry
    return $ blankLines ++ dayEntries

newtype LogExample = LogExample String deriving (Show)
instance Arbitrary LogExample where
    arbitrary = LogExample <$> genLog

propLog :: LogExample -> Bool
propLog (LogExample s) = case parseString parseLog mempty s of
    Text.Trifecta.Success _ -> True
    _ -> False

main :: IO ()
main = do
    quickCheck propLog