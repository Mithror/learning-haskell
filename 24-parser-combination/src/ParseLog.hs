{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ParseLog where

import Control.Applicative ((<|>), liftA2)
import Data.Monoid ((<>))
import Text.Trifecta
import Data.Time.Format
import Data.Time
import Data.List (intersperse)
import Text.RawString.QQ

import Test.QuickCheck

data LogEntry = LogEntry TimeOfDay String deriving Eq
instance Show LogEntry where
    show (LogEntry t s) =
        let time = formatTime defaultTimeLocale "%H:%M" t
        in time ++ " " ++ s
instance Arbitrary LogEntry where
    arbitrary = do
        tod <- TimeOfDay <$> choose (0,23) <*> choose (0,59) <*> pure 0
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

skipBlankLines :: Parser ()
skipBlankLines =
    skipMany $ (skipWhiteSpace >> skipComment >> newline) <|>
            (skipWhiteSpace >> newline)

skipComment :: Parser ()
skipComment = skipMany $ do
    _ <- count 2 $ char '-'
    skipMany $ notChar '\n'

skipWhiteSpace :: Parser ()
skipWhiteSpace = skipMany $ oneOf "\t "

removeComment :: String -> String
removeComment [] = []
removeComment (x:xs) = go "" x False xs
    where go s '-' True _ = s -- skip comment
          go s '-' False [] = s ++ "-" -- ended with '-'
          go s c _ [] = s ++ [c] -- ended without comment
          go s '-' False (y:ys) = go s y True ys -- possible start of comment
          go s c _ (y:ys) = go (s ++ [c]) y False ys


parseLogEntry :: Parser LogEntry
parseLogEntry = do
    t <- count 2 digit <> string ":" <> count 2 digit 
    let m = parseTimeM True defaultTimeLocale "%H:%M" t
    timeOfDay <- case m of
        Just time -> return time
        Nothing -> unexpected "Incorrect time format"
    _ <- space
    s <- some $ notChar '\n'
    _ <- newline
    return $ LogEntry timeOfDay (removeComment s)

parseDayEntry :: Parser DayEntry
parseDayEntry = do
    _ <- char '#'
    _ <- space
    d <- some digit <> string "-" 
           <> count 2 digit <> string "-" 
           <> count 2 digit
    let m = parseTimeM True defaultTimeLocale "%Y-%m-%d" d
    day <- case m of
        Just day' -> return day'
        Nothing -> unexpected "Incorrect day format"
    -- only thing allowed after day is whitespace and comments
    skipWhiteSpace
    skipComment
    _ <- newline
    -- Followed by a list of paserLogEntries
    logEntries <- many parseLogEntry
    skipBlankLines
    return $ DayEntry day logEntries 

parseLog :: Parser Log
parseLog  = do
    skipBlankLines
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