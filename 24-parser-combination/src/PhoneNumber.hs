{-# LANGUAGE OverloadedStrings #-}

module PhoneNumber where

import Control.Applicative ((<|>))
import Control.Monad (replicateM)

import Text.Trifecta
import Test.Hspec

import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Property as QP
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Data.Maybe (isJust)
import Data.Char (isDigit)

-- This is an exercise for the Belgian Phone numbers system
-- From wikipedia
{-
Belgian telephone numbers consist of 3 major parts: First '0', second the
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
    let mobile  = do
            one <- char '4'
            two <- oneOf ['6', '7', '8', '9']
            three <- digit
            return $ MobileZP [one, two, three]
        landOne = LandOneZP . (:[]) <$> oneOf ['2', '3', '4', '9']
        landTwo = LandTwoZP <$> count 2 digit
    -- Order is important, we want to check mobile first, followed by
    -- landOne. Everything else is assumed to be land two
    in try mobile <|> landOne <|> landTwo

parseSNOfSix :: Parser String
parseSNOfSix = do
    let p = count 2 digit
        skip = skipOptional $ char ' ' <|> char '.'
    p1 <- p
    skip
    p2 <- p
    skip
    p3 <- p
    return $ p1 ++ p2 ++ p3

parseSNOfSeven :: Parser String
parseSNOfSeven = do
    let p = count 2 digit
        skip = skipOptional $ char ' ' <|> char '.'
    p1 <- count 3 digit
    skip
    p2 <- p
    skip
    p3 <- p
    return $ p1 ++ p2 ++ p3

parsePhone :: Parser PhoneNumberBE
parsePhone = do
    -- Country code
    m <- optional $ string "+32"
    -- white space
    skipMany $ char ' '
    -- Leading zero depends on country code
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
            MobileZP _  -> MobileSN <$> parseSNOfSix
            LandOneZP _ -> LandOneSN <$> parseSNOfSix
            LandTwoZP _ -> LandTwoSN <$> parseSNOfSeven
    return $ PhoneNumberBE z s

-- Tests

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

-- ZonePrefix
validMobileZonePrefix :: [String]
validMobileZonePrefix =
    [(++) "4"] <*> (map ((++) . show) [6..9] <*> map show [0..9])

validLandOneZonePrefix :: [String]
validLandOneZonePrefix = ["2", "3", "4", "9"]

validLandTwoZonePrefix :: [String]
validLandTwoZonePrefix =
    map (++) ["1", "5", "6", "7", "8"] <*> map show [0..9]

invalidZonePrefix :: [String]
invalidZonePrefix = ["0", "a"]

validMobileZonePrefixTest :: (String -> ZonePrefix) -> String -> SpecWith ()
validMobileZonePrefixTest zp input = do
    it ("Zone prefix: " ++ input) $ do
        let m = parseString parseZonePrefix mempty input
            r = maybeSuccess m
        r `shouldBe` Just (zp input)

invalidZonePrefixTest :: String -> SpecWith ()
invalidZonePrefixTest input = do
    it ("Zone prefix: " ++ input) $ do
        let m = parseString parseZonePrefix mempty input
            r = maybeSuccess m
        r `shouldBe` Nothing

zonePrefixTests :: Spec
zonePrefixTests = do
    describe "Parsing valid Mobile Zone Prefixes" $ do
        mapM_ (validMobileZonePrefixTest MobileZP) validMobileZonePrefix
    describe "Parsing valid Land One Zone Prefixes" $ do
        mapM_ (validMobileZonePrefixTest LandOneZP) validLandOneZonePrefix
    describe "Parsing valid Land Two Zone Prefixes" $ do
        mapM_ (validMobileZonePrefixTest LandTwoZP) validLandTwoZonePrefix
    describe "Parsing invalid Zone Prefixes" $ do
        mapM_ invalidZonePrefixTest invalidZonePrefix

gen_MobileZP :: Gen String
gen_MobileZP = do
    c1 <- elements ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    c2 <- elements ['6', '7', '8', '9']
    return $ ['4', c2, c1]

gen_LandOneZP :: Gen String
gen_LandOneZP = elements ["2", "3", "4", "9"]

gen_LandTwoZP :: Gen String
gen_LandTwoZP = do
    c1 <- elements ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    c2 <- elements ['1', '5', '6', '7', '8']
    return $ [c2, c1]

prop_ZP :: Gen String -> (String -> ZonePrefix) -> QP.Property
prop_ZP g c = QP.forAll g $ \input ->
    let r = maybeSuccess m
        m = parseString parseZonePrefix mempty input
    in isJust r

-- Subscriber Number
gen_MobileSN :: Gen String
gen_MobileSN = do
    let c = elements ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    p1 <- replicateM 2 c
    r <- replicateM 2 $ do
        s1 <- elements [" ",  "."]
        p2 <- replicateM 2 c
        return $ s1 ++ p2
    return $ p1 ++ (concat r)

gen_LandOneSN :: String -> Gen String
gen_LandOneSN zp = do
    let c = elements ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    c1 <- case zp of
            "4" -> elements ['0', '1', '2', '3', '4', '5']
            otherwise -> c
    c2 <- c
    r <- replicateM 2 $ do
        s1 <- elements [" ",  "."]
        p2 <- replicateM 2 c
        return $ s1 ++ p2
    return $ c1:c2:(concat r)

gen_LandOneSN' :: Gen String
gen_LandOneSN' = do
    zp <- gen_LandOneZP
    gen_LandOneSN zp

gen_LandTwoSN :: Gen String
gen_LandTwoSN = do
    let c = elements ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    p1 <- replicateM 3 c
    r <- replicateM 2 $ do
        s1 <- elements [" ",  "."]
        p2 <- replicateM 2 c
        return $ s1 ++ p2
    return $ p1 ++ (concat r)

prop_SN :: Gen String -> Parser String -> QP.Property
prop_SN g p = QP.forAll g $ \input ->
    let r = maybeSuccess m
        m = parseString p mempty input
    in isJust r

-- Phone Number
gen_Spaces :: Gen String
gen_Spaces = listOf $ return ' '

gen_CountryCode :: Gen String
gen_CountryCode = do
    sp <- gen_Spaces
    return $ "+32" ++ sp

gen_NoCountryCode :: Gen String
gen_NoCountryCode = do
    sp <- gen_Spaces
    return $ sp ++ "0"

gen_MobileNumber :: Gen String
gen_MobileNumber = do
    cc <- oneof [gen_CountryCode, gen_NoCountryCode]
    zp <- gen_MobileZP
    sep <- elements ["/", " ", ""]
    sn <- gen_MobileSN
    return $ cc ++ zp ++ sep ++ sn

gen_LandOneNumber :: Gen String
gen_LandOneNumber = do
    cc <- oneof [gen_CountryCode, gen_NoCountryCode]
    zp <- gen_LandOneZP
    sep <- elements ["/", " ", ""]
    sn <- gen_LandOneSN zp
    return $ cc ++ zp ++ sep ++ sn

gen_LandTwoNumber :: Gen String
gen_LandTwoNumber = do
    cc <- oneof [gen_CountryCode, gen_NoCountryCode]
    zp <- gen_LandTwoZP
    sep <- elements ["/", " ", ""]
    sn <- gen_LandTwoSN
    return $ cc ++ zp ++ sep ++ sn

prop_PhoneNumberBE :: QP.Property
prop_PhoneNumberBE = QP.forAll g $ \input ->
        let r = maybeSuccess m
            m = parseString parsePhone mempty input
        in isJust r
    where g = oneof [gen_MobileNumber, gen_LandOneNumber, gen_LandTwoNumber]

main :: IO ()
main = do
    hspec $ do
        zonePrefixTests
    Q.quickCheck $ prop_ZP gen_MobileZP MobileZP
    Q.quickCheck $ prop_ZP gen_LandOneZP LandOneZP
    Q.quickCheck $ prop_ZP gen_LandTwoZP LandTwoZP
    Q.quickCheck $ prop_SN gen_MobileSN parseSNOfSix
    Q.quickCheck $ prop_SN gen_LandOneSN' parseSNOfSix
    Q.quickCheck $ prop_SN gen_LandTwoSN parseSNOfSeven
    Q.quickCheckWith Q.stdArgs { Q.maxSuccess = 10000 } $ prop_PhoneNumberBE