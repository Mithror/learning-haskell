{-# LANGUAGE OverloadedStrings #-}

module IPAddress where

import Control.Applicative ((<|>), liftA2, liftA3)
import Text.Trifecta hiding (span)
import Data.Word (Word32, Word64)
import Data.Bits (shiftL, shiftR, (.&.))
import Control.Monad (replicateM)
import Text.Parser.Char (char)
import Data.Char (toLower, ord)
import Data.List (intersperse, span)
import Numeric (showHex)

import Test.Hspec

-- Exercise 6
data IPAddress = IPAddress Word32 deriving (Eq, Ord)

parseDecOctet :: CharParsing m => m Integer
parseDecOctet =
    fmap read $ case1 <|> case2 <|>  case3 <|> case4 <|> case5
    where case1 = try $ sequenceA [char '2', char '5', oneOf "012345"]
          case2 = try $ sequenceA [char '2', oneOf "01234", digit]
          case3 = try $ sequenceA [char '1', digit, digit]
          case4 = try $ sequenceA [oneOf "123456789", digit]
          case5 = pure <$> digit

shiftLFolder :: Int -> [Integer] -> Integer
shiftLFolder i = fst . foldr f (0, 0)
    where f a (t, s) = ((a `shiftL` s) + t, s + i)

parseIPv4Address :: CharParsing m => m IPAddress
parseIPv4Address =
    let wDot = char '.' *> parseDecOctet
        words = liftA2 (:) parseDecOctet $ replicateM 3 wDot
    in IPAddress . fromInteger . shiftLFolder 8 <$> words

-- Exercise 7
data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord)

-- From RFC 3986

-- IPv6address   =                            6( h16 ":" ) ls32
-- /                       "::" 5( h16 ":" ) ls32
-- / [               h16 ] "::" 4( h16 ":" ) ls32
-- / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
-- / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
-- / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
-- / [ *4( h16 ":" ) h16 ] "::"              ls32
-- / [ *5( h16 ":" ) h16 ] "::"              h16
-- / [ *6( h16 ":" ) h16 ] "::"
-- h16           = 1*4HEXDIG
-- ls32          = ( h16 ":" h16 ) / IPv4address

-- "::" = Cons
data IPv6ParseOption =
    NoCons | Cons0 | Cons1 | Cons2 | Cons3 | Cons4 | Cons5 | Cons6 | Cons7

-- This is just the representation of the ABNF from the RFC
parseIPv6Address :: Parser IPAddress6
parseIPv6Address = fmap f $ (try $ parseOption NoCons)
                 <|> (try $ parseOption Cons0)
                 <|> (try $ parseOption Cons1)
                 <|> (try $ parseOption Cons2)
                 <|> (try $ parseOption Cons3)
                 <|> (try $ parseOption Cons4)
                 <|> (try $ parseOption Cons5)
                 <|> (try $ parseOption Cons6)
                 <|> parseOption Cons7
    where f = integerToIPv6Address . shiftLFolder 16

integerToIPv6Address :: Integer -> IPAddress6
integerToIPv6Address n =
    let [w1, w2] = fmap f [n `shiftR` 64, n]
        f = fromInteger . (.&. 0xFFFFFFFFFFFFFFFF)
    in IPAddress6 w1 w2

parseOption :: IPv6ParseOption -> Parser [Integer]
parseOption o = liftA3 f (parseFirst o) (parseMiddle o) (parseLast o)
    where f s1 s2 s3 = s1 ++ s2 ++ s3

parseFirst :: IPv6ParseOption -> Parser [Integer]
parseFirst o = case o of
    NoCons -> pure []
    Cons0 -> string "::" *> pure [0]
    Cons1 -> (try $ lower Cons0) <|> go 0
    Cons2 -> (try $ lower Cons1) <|> go 1
    Cons3 -> (try $ lower Cons2) <|> go 2
    Cons4 -> (try $ lower Cons3) <|> go 3
    Cons5 -> (try $ lower Cons4) <|> go 4
    Cons6 -> (try $ lower Cons5) <|> go 5
    Cons7 -> (try $ lower Cons6) <|> go 6
    where lower o' = liftA2 (++) (parseFirst o') (pure [0])
          go n = liftA2 (++) (rep n) (parseFirst Cons0)
          rep n = liftA2 (++) (replicateM n piece) end
          piece = parseH16 <* char ':'
          end = sequenceA [parseH16]

parseMiddle :: IPv6ParseOption -> Parser [Integer]
parseMiddle o = case o of
    NoCons -> rep 6
    Cons0 -> rep 5
    Cons1 -> rep 4
    Cons2 -> rep 3
    Cons3 -> rep 2
    Cons4 -> rep 1
    otherwise -> pure []
    where rep n = replicateM n (parseH16 <* char ':')

parseLast :: IPv6ParseOption -> Parser [Integer]
paresLast Cons6 = sequenceA [parseH16]
parseLast Cons7 = pure []
parseLast _ = parseLs32

parseLs32 :: Parser [Integer]
parseLs32 = try case1 <|> case2
    where case1 = sequenceA [parseH16 <* char ':', parseH16]
          case2 = toWord16s <$> parseIPv4Address
          toWord16s (IPAddress w) =
            let w' = toInteger w
            in fmap (.&. 0xFFFF) [w' `shiftR` 16, w']

parseH16 :: Parser Integer
parseH16 = fmap (shiftLFolder 4) $ try four <|> try three <|> try two <|> one
    where four = replicateM 4 parseHexDig
          three = replicateM 3 parseHexDig
          two = replicateM 2 parseHexDig
          one = replicateM 1 parseHexDig

parseHexDig :: Parser Integer
parseHexDig = d <|> h
    where d = read <$> sequenceA [digit]
          h = f . toLower <$> oneOf "abcdefABCDEF"
          f c = toInteger $ ord c - ord 'a' + 10

-- Exercise 8
instance Show IPAddress where
    show (IPAddress w32) =
        let n = toInteger w32
        in show ((n `shiftR` 24) .&. 0xFF) ++ "." ++
           show ((n `shiftR` 16) .&. 0xFF) ++ "." ++
           show ((n `shiftR` 8)  .&. 0xFF) ++ "." ++
           show (n .&. 0xFF)

-- RFC 5952
instance Show IPAddress6 where
    show i = concat $ intersperse ":" $ processed
        where processed = fmap fix longest
              longest = repLongestListOfZeroes $ collapseZeroes asStrings
              asStrings = fmap (flip showHex "") (toArray i)
              fix x = if length x > 1 then intersperse ':' x else x

toArray :: IPAddress6 -> [Integer]
toArray (IPAddress6 w1 w2) = go w1 ++ go w2
    where go w = reverse $ take 4 $ f (toInteger w)
          f w = w .&. mask : f (w `shiftR` 16)
          mask = 0xFFFF

collapseZeroes :: [String] -> [String]
collapseZeroes [] = []
collapseZeroes xs = nonZeroes ++ zeroes ++ remainder
    where (nonZeroes, rest) = span (/= "0") xs
          (zeroes', rest') = span (== "0") rest
          zeroes = [concat zeroes']
          remainder = collapseZeroes rest'

repLongestListOfZeroes :: [String] -> [String]
repLongestListOfZeroes xs = h ++ rep t
    where (h, t) = span (p . length) xs
          p x = (max' < 2) || (x /= max')
          max' = maximum $ fmap length xs
          rep [] = []
          rep (x:xs) = "":xs

-- Exercise 9
toIPAddress6 :: IPAddress -> IPAddress6
toIPAddress6 (IPAddress w32) = IPAddress6 0 (fromIntegral w32)

toIPAddress :: IPAddress6 -> Maybe IPAddress
toIPAddress (IPAddress6 w64 w64')
    | w64 /= 0 = Nothing
    | w64' > (fromIntegral $ (maxBound :: Word32)) = Nothing
    | otherwise = Just $ IPAddress (fromIntegral w64')

-- Tests
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing
-- IPv4
testDecOctetHelper :: String -> Maybe Integer -> Expectation
testDecOctetHelper s r = do
    let m = parseString parseDecOctet mempty s
        r' = maybeSuccess m
    r' `shouldBe` r

testIPv4Helper :: String -> Maybe IPAddress -> Expectation
testIPv4Helper s r = do
    let m = parseString parseIPv4Address mempty s
        r' = maybeSuccess m
    r' `shouldBe` r

testDecOctet :: IO ()
testDecOctet = hspec $ do
    describe "Test parsing of byte" $ do
        it "Can parse 0" $ testDecOctetHelper "0" (Just 0)
        it "Can parse 10" $ testDecOctetHelper "10" (Just 10)
        it "Can parse 100" $ testDecOctetHelper "100" (Just 100)
        it "Can parse 249" $ testDecOctetHelper "249" (Just 249)
        it "Can parse 255" $ testDecOctetHelper "255" (Just 255)
        it "Can parse 260 as 26" $ testDecOctetHelper "256" (Just 25)
        it "Can parse 256 as 25" $ testDecOctetHelper "256" (Just 25)
        it "Can parse 2550 as 255" $ testDecOctetHelper "2550s" (Just 255)
        it "Can't parse -1" $ testDecOctetHelper "-1" Nothing
        it "Can't parse empty" $ testDecOctetHelper "" Nothing

testIPv4 :: IO ()
testIPv4 = hspec $ do
    describe "Test parsing of IPv4" $ do
        it "Can parse 0.0.0.0" $ do
            testIPv4Helper "0.0.0.0" (Just (IPAddress 0))
        it "Can parse 255.255.255.255" $ do
            testIPv4Helper "255.255.255.255" (Just (IPAddress (2^32 - 1)))
        it "Can parse 0.0.0.255.1" $ do
            testIPv4Helper "0.0.0.255.1" (Just (IPAddress 255))
        it "Can't parse .0.0.0.0" $ do
            testIPv4Helper ".0.0.0.0" Nothing
        it "Can't parse 0..0.0.0" $ do
            testIPv4Helper "0..0.0.0" Nothing

-- IPv6
testParseFirstHelper :: IPv6ParseOption
                     -> String
                     -> Maybe [Integer]
                     -> Expectation
testParseFirstHelper o s r = do
    let m = parseString (parseFirst o) mempty s
        r' = maybeSuccess m
    r' `shouldBe` r

testParseFirst :: IO ()
testParseFirst = hspec $ do
    describe "Test parsing of parseFirst NoCons" $ do
        it "Can parse anything" $ do
            testParseFirstHelper NoCons "" (Just [])
    describe "Test parsing of parseFirst Cons0" $ do
        it "Can parse only '::'" $ do
            testParseFirstHelper Cons0 "::" (Just [0])
        it "Cannot parse non '::'" $ do
            testParseFirstHelper Cons0 "a::" (Nothing)
    describe "Test parsing of parseFirst Cons1" $ do
        it "Can parse only '::'" $ do
            testParseFirstHelper Cons1 "::" (Just [0, 0])
        it "Can parse 'f::'" $ do
            testParseFirstHelper Cons1 "f::" (Just [15,0])
        it "Cannot parse f:f::" $ do
            testParseFirstHelper Cons1 "f:f::" Nothing
    describe "Test parsing of parseFirst Cons2" $ do
        it "Can parse only '::'" $ do
            testParseFirstHelper Cons2 "::" (Just [0, 0, 0])
        it "Can parse 'f::'" $ do
            testParseFirstHelper Cons2 "f::" (Just [15,0,0])
        it "Can parse 'f:f::'" $ do
            testParseFirstHelper Cons2 "f:f::" (Just [15,15,0])
        it "Cannot parse f:f:f::" $ do
            testParseFirstHelper Cons2 "f:f:f::" Nothing
    describe "Test parsing of parseFirst Cons3" $ do
        it "Can parse only '::'" $ do
            testParseFirstHelper Cons3 "::" (Just [0, 0, 0, 0])
        it "Can parse 'f::'" $ do
            testParseFirstHelper Cons3 "f::" (Just [15,0,0,0])
        it "Can parse 'f:f::'" $ do
            testParseFirstHelper Cons3 "f:f::" (Just [15,15,0,0])
        it "Can parse 'f:f:f::'" $ do
            testParseFirstHelper Cons3 "f:f:f::" (Just [15,15,15,0])
        it "Cannot parse f:f:f:f::" $ do
            testParseFirstHelper Cons3 "f:f:f:f::" Nothing
    describe "Test parsing of parseFirst Cons4" $ do
        it "Can parse only '::'" $ do
            testParseFirstHelper Cons4 "::" (Just [0, 0, 0, 0, 0])
        it "Can parse 'f::'" $ do
            testParseFirstHelper Cons4 "f::" (Just [15,0,0,0, 0])
        it "Can parse 'f:f::'" $ do
            testParseFirstHelper Cons4 "f:f::" (Just [15,15,0,0,0])
        it "Can parse 'f:f:f::'" $ do
            testParseFirstHelper Cons4 "f:f:f::" (Just [15,15,15,0, 0])
        it "Can parse f:f:f:f::" $ do
            testParseFirstHelper Cons4 "f:f:f:f::" (Just [15,15,15,15, 0])
        it "Cannot parse f:f:f:f:f::" $ do
            testParseFirstHelper Cons4 "f:f:f:f:f::" Nothing
    describe "Test parsing of parseFirst Cons5" $ do
        it "Can parse only '::'" $ do
            testParseFirstHelper Cons5 "::" (Just [0, 0, 0, 0, 0, 0])
        it "Can parse 'f::'" $ do
            testParseFirstHelper Cons5 "f::" (Just [15,0,0,0, 0, 0])
        it "Can parse 'f:f::'" $ do
            testParseFirstHelper Cons5 "f:f::" (Just [15,15,0,0,0, 0])
        it "Can parse 'f:f:f::'" $ do
            testParseFirstHelper Cons5 "f:f:f::" (Just [15,15,15,0, 0, 0])
        it "Can parse f:f:f:f::" $ do
            testParseFirstHelper Cons5 "f:f:f:f::" (Just [15,15,15,15, 0, 0])
        it "Can parse f:f:f:f:f::" $ do
            testParseFirstHelper Cons5 "f:f:f:f:f::" (Just [15,15,15,15,15, 0])
        it "Cannot parse f:f:f:f:f:f::" $ do
            testParseFirstHelper Cons5 "f:f:f:f:f:f::" Nothing
    -- rest is copy-paste but also not necessary since they're all
    -- handled the same way in the parseFirst method.


testIPv6Helper :: String -> Maybe IPAddress6 -> Expectation
testIPv6Helper s r = do
    let m = parseString parseIPv6Address mempty s
        r' = maybeSuccess m
    r' `shouldBe` r

testIPv6 :: IO ()
testIPv6 = hspec $ do
    describe "Test parsing of IPv6" $ do
        it "Can parse 0:0:1:0:0:0:1:0" $ do
            testIPv6Helper "0:0:1:0:0:0:1:0" (Just (IPAddress6 65536 65536))
        it "Can parse ::0:1:0:0:0:1:0" $ do
            testIPv6Helper "::0:1:0:0:0:1:0" (Just (IPAddress6 65536 65536))
        it "Can parse ::1:0:0:0:1:0" $ do
            testIPv6Helper "::1:0:0:0:1:0" (Just (IPAddress6 65536 65536))
        it "Can parse 0::1:0:0:0:1:0" $ do
            testIPv6Helper "0::1:0:0:0:1:0" (Just (IPAddress6 65536 65536))