{-# LANGUAGE OverloadedStrings #-}

module PhoneNumber where

import Control.Applicative ((<|>))
import Text.Trifecta
import Test.Hspec

-- This is an exercise for the Belgian Phone numbers system
-- From wikipedia
{-
Belgian telephone numbers consist of two major parts: Firstly '0', secondly the 
"zone prefix" (A) which is 1 or 2 digits long for landlines and 3 digits long 
for mobile phones and thirdly the "subscriber's number" (B).

Land lines are always 9 digits long. They are prefixed by a zero, followed by 
the zone prefix. Depending on the length of the zone prefix, the subscriber's 
number consists of either 6 or 7 digits. Hence land line numbers are written 
either 0AA BB BB BB or 0A BBB BB BB.

Mobile Phone numbers always consist of 10 digits. The first digit of the "zone 
prefix" of a mobile number is always '4'. Then follows 2 digits indicating to 
which Mobile Operator's pool the number originally belonged when it was taken 
into usage. The fourth digit represents a "sub-group" of this pool and has no 
additional meaning other than increasing the amount of possible numbers. The 
subscriber's number consists of 6 digits. Hence, mobile phone numbers are 
written 04AA BB BB BB. Sometimes, the last 6 digits are written in two groups 
of 3 digits to increase readability: 04AA BBB BBB.

Numbers are sometimes written with a slash in between the zone prefix and the 
subscriber's number. This is the case for both land lines and mobile phone 
numbers. Sometimes, dots are written between the blocks of the subscriber's 
number. Examples: 0AA/BB BB BB, 0AA/BB.BB.BB; for mobile numbers: 
04AA/BB BB BB, 04AA/BB.BB.BB or 04AA/BBB.BBB.

The international country code prefix for Belgium is "+32". When dialing a 
number with the prefix, the 0 can be dropped, e.g.: +32 4AA BB BB BB.
-}

-- see also https://en.wikipedia.org/wiki/Telephone_numbers_in_Belgium


-- Will only look at landlines and phonelines
-- not special numbers or non-geographic numbers
data ZonePrefix = MobileZP String 
                | LandOneZP String 
                | LandTwoZP String
                deriving (Eq)
instance Show ZonePrefix where
    show (MobileZP s)  = "0" ++ s
    show (LandOneZP s) = "  0" ++ s
    show (LandTwoZP s) = " 0" ++ s

data SubscribersNumber = MobileSN String 
                       | LandOneSN String 
                       | LandTwoSN String
                       deriving (Eq)
instance Show SubscribersNumber where
    show (LandOneSN (a:b:c:d:e:f:g:[])) = a:b:c:" " ++ d:e:" " ++ f:g:[]
    show (LandOneSN s) = "Invalid format: " ++ s
    show (MobileSN (a:b:c:d:e:f:[])) = " " ++ a:b:" " ++ c:d:" " ++ e:f:[]
    show (MobileSN s) = "Invalid format: " ++ s
    show (LandTwoSN (a:b:c:d:e:f:[])) = " " ++ a:b:" " ++ c:d:" " ++ e:f:[]
    show (LandTwoSN s) = "Invalid format: " ++ s

data PhoneNumberBE = PhoneNumberBE ZonePrefix SubscribersNumber deriving Eq
instance Show PhoneNumberBE where
    show (PhoneNumberBE z s) = "(PhoneNumberBE " ++ show z 
                                                 ++ " " 
                                                 ++ show s 
                                                 ++ ")"


-- Mobile numbers are: 046x 047x 048x 049x
-- Land lines are most other things where:
--  02,03,04 and 09 are single digit land line area codes
--  the rest are two digit land line phone
parseZonePrefix :: Parser ZonePrefix
parseZonePrefix =
    let mobile  = MobileZP <$> do
                    one <- char '4'
                    two <- oneOf ['6', '7', '8', '9']
                    three <- digit
                    return $ [one, two, three]
        landOne = LandOneZP . (:[]) <$> oneOf ['2', '3', '4', '9']
        landTwo = LandTwoZP <$> count 2 digit
    -- Order is important, we want to check mobile first, followed by
    -- landOne. Everything else is assumed to be land two
    in mobile <|> landOne <|> landTwo

parseMobileSN :: Parser SubscribersNumber
parseMobileSN = do
    let p = count 2 digit
        skip = skipOptional $ char ' ' <|> char '.'
    p1 <- p
    skip
    p2 <- p
    skip
    p3 <- p
    return . MobileSN $ p1 ++ p2 ++ p3

parseLandTwoSN :: Parser SubscribersNumber
parseLandTwoSN = do
    let p = count 2 digit
        skip = skipOptional $ char ' ' <|> char '.'
    p1 <- p
    skip
    p2 <- p
    skip
    p3 <- p
    return . LandTwoSN $ p1 ++ p2 ++ p3

parseLandOneSN :: Parser SubscribersNumber
parseLandOneSN = do
    let p = count 2 digit
        skip = skipOptional $ char ' ' <|> char '.'
    p1 <- count 3 digit
    skip
    p2 <- p
    skip
    p3 <- p
    return . LandTwoSN $ p1 ++ p2 ++ p3

parsePhone :: Parser PhoneNumberBE
parsePhone = do
    -- Country code
    m <- optional $ string "+32"
    -- white space
    skipMany $ char ' '
    -- Leading zero depends on country code
    skipOptional $ char ' ' 
    _ <- case m of
        Just _  -> do
                    a <- optional $ char '0'
                    case a of
                        Nothing -> return Nothing
                        Just _ -> unexpected "0 not expected here"
        Nothing -> Just <$> char '0'
    z <- parseZonePrefix
    skipOptional $ char ' ' <|> char '/'
    s <- case z of
            MobileZP _  -> parseMobileSN
            LandOneZP _ -> parseLandOneSN
            LandTwoZP _ -> parseLandTwoSN
    return $ PhoneNumberBE z s
    
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

-- Should test all options for mobile and single-digit land lines,
-- but that is just copy-paste... QuickCheck could also help.
main :: IO ()
main = hspec $ do
    describe "Parse Zone Prefix" $ do
        it "Mobile starts with 4[6-9]x " $ do
            let m = parseString parseZonePrefix mempty "472"
                r = maybeSuccess m
            r `shouldBe` Just (MobileZP "472")
        it "Incorrect mobile zone prefix should fail" $ do
            let m = parseString parseZonePrefix mempty "47"
                r = maybeSuccess m
            r `shouldBe` Nothing
        it "Land line with single digit" $ do
            let m = parseString parseZonePrefix mempty "2"
                r = maybeSuccess m
            r `shouldBe` Just (LandOneZP "2")
        it "Land line with double digit" $ do
            let m = parseString parseZonePrefix mempty "55"
                r = maybeSuccess m
            r `shouldBe` Just (LandTwoZP "55")
    -- ...