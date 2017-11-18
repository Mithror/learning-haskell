{-# LANGUAGE OverloadedStrings #-}

module IPAddress where

import Text.Trifecta hiding (span)
import Data.Word
import Data.Bits
import Data.List (splitAt)
import Numeric (readHex, showHex)
import Control.Monad.State
import Data.Maybe (fromMaybe)
-- import Data.List (group, span)
import Data.Either (lefts)
-- import qualified Data.Sequence as S

import Test.Hspec

-- Exercise 6
data IPAddress = IPAddress Word32 deriving (Eq, Ord)
instance Show IPAddress where
    show (IPAddress w32) =
        let n = toInteger w32
        in show ((n `shiftR` 24) .&. 0xFF) ++ "." ++
           show ((n `shiftR` 16) .&. 0xFF) ++ "." ++
           show ((n `shiftR` 8)  .&. 0xFF) ++ "." ++
           show (n .&. 0xFF)

parseValidIPByte :: Parser Word8
parseValidIPByte = do
    i <- decimal 
    let max' = toInteger (maxBound :: Word8)
        min' = toInteger (minBound :: Word8)
    if i <= max' && i >= min'
    then return (fromInteger i)
    else unexpected "Invalid byte"

parseIPv4Address :: Parser IPAddress
parseIPv4Address = do
    b1 <- toInteger <$> parseValidIPByte
    _  <- char '.'
    b2 <- toInteger <$> parseValidIPByte
    _  <- char '.'
    b3 <- toInteger <$> parseValidIPByte
    _  <- char '.'
    b4 <- toInteger <$> parseValidIPByte
    return $ IPAddress . fromInteger $ (b1 `shiftL` 24) +
                                       (b2 `shiftL` 16) +
                                       (b3 `shiftL`  8) +
                                       b4

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

testByteHelper :: String -> Maybe Word8 -> Expectation
testByteHelper s r = do
    let m = parseString parseValidIPByte mempty s
        r' = maybeSuccess m
    r' `shouldBe` r

testIPv4Helper :: String -> Maybe IPAddress -> Expectation
testIPv4Helper s r = do
    let m = parseString parseIPv4Address mempty s
        r' = maybeSuccess m
    r' `shouldBe` r

testByte :: IO ()
testByte = hspec $ do
    describe "Test parsing of byte" $ do
        it "Can parse 0" $ testByteHelper "0" (Just 0)
        it "Can parse 255" $ testByteHelper "255" (Just 255)
        it "Can't parse 256" $ testByteHelper "256" Nothing
        it "Can't parse -1" $ testByteHelper "-1" Nothing
        it "Can't parse empty" $ testByteHelper "" Nothing    

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

toIPAddress6 :: IPAddress -> IPAddress6
toIPAddress6 (IPAddress w32) = IPAddress6 0 (fromIntegral w32)

-- Exercise 7

-- Datatypes:
data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord)
-- An IPv6Piece is the part between ':', it can be either
-- "::" or "ABCD"
type IPv6Piece = Either () Word16

--
toArray :: IPAddress6 -> [Word16]
toArray (IPAddress6 w64 w64') =
    let n = toInteger w64
        m = toInteger w64'
        mask = fromInteger . (\a -> (.&.) a 0xFFFF)
    in mask (n `shiftR` 48) :
       mask (n `shiftR` 32) :
       mask (n `shiftR` 16) :
       mask n :
       mask (m `shiftR` 48) :
       mask (m `shiftR` 32) :
       mask (m `shiftR` 16) :
       mask m : []

toArray' :: [Word16] -> [Either Int Word16]
toArray' [] = []
toArray' s@(0:0:_) = (Left $ length ys) : (toArray' zs)
    where (ys, zs) = span (==0) s
toArray' (x:xs) = (Right x) : toArray' xs

findMax :: [Either Int Word16] -> Maybe Int
findMax xs = go $ lefts xs
    where go [] = Nothing
          go ys = Just $ maximum ys

replaceLeft :: Int -> [Either Int Word16] -> [IPv6Piece]
replaceLeft m xs = go m xs False
    where go _ [] _ = []
          go i ((Right c):ys) b = (Right c) : (go i ys b)
          go i ((Left c):ys) False = 
            if (c == i)
            then (Left ()) : (go i ys True)
            else (replicate c (Right 0)) ++ (go i ys False)
          go i ((Left c):ys) True = (replicate c (Right 0)) ++ (go i ys True)

showPieces :: [IPv6Piece] -> String
showPieces [] = ""
showPieces ((Right c):ys) = (showHex c "") ++ (go ys)
    where go [] = []
          go (Right c':zs) = ":" ++ (showHex c' "") ++ (go zs)
          go zs = showPieces zs
showPieces ((Left ():ys)) = "::" ++ (showPieces ys)
            

instance Show IPAddress6 where
    show p = let xs = toArray' (toArray p)
             in showPieces $ (case findMax xs of
                                     Nothing -> replaceLeft 0 xs
                                     (Just i) -> replaceLeft i xs)

validHex :: String
validHex = "abcdefABCDEF0123456789"

-- Helper functions
-- only parses [0-9A-Fa-f]{1,4}
readWord16 :: String -> Maybe Word16
readWord16 s =
    let r = readHex  (take 4 s) :: [(Word16, String)]
    in case r of
        ((w,s'):[]) -> if s' == "" then Just w else Nothing
        _ -> Nothing

mkWord64 :: Word16 -> Word16 -> Word16 -> Word16 -> Word64
mkWord64 a b c d = (fromIntegral a `shiftL` 48) +
                   (fromIntegral b `shiftL` 32) +
                   (fromIntegral c `shiftL` 16) +
                   (fromIntegral d)

mkIPAddress6 :: [Word16] -> Maybe IPAddress6
mkIPAddress6 xs = 
    case length xs of
        8 -> let (a,b) = splitAt 4 xs
                 w1 = (mkWord64 (a !! 0) (a !! 1) (a !! 2) (a !! 3))
                 w2 = (mkWord64 (b !! 0) (b !! 1) (b !! 2) (b !! 3))
             in Just $ IPAddress6 w1 w2                             
        _ -> Nothing

expandIPv6Pieces :: [IPv6Piece] -> Maybe [Word16]
expandIPv6Pieces xs =
    case length $ filter (==(Left ())) xs of
        0 -> Just $ foldr f [] xs
        1 -> Just $ foldr g [] xs
        _ -> Nothing
    where f (Left _)  s = s
          f (Right c) s = c:s
          n = max (9 - (length xs)) 0
          g (Left _)  s = (take n $ repeat 0) ++ s
          g (Right c) s = c:s

getIPAddress6 :: [IPv6Piece] -> Maybe IPAddress6
getIPAddress6 xs = expandIPv6Pieces xs >>= mkIPAddress6

type IPv6State = Bool
type IPv6Parser a = StateT IPv6State Parser a

-- Parsing the individual pieces:
-- hex, 1-4 hexes, ':', '::'
parseSingleHex :: IPv6Parser Char
parseSingleHex = do
    c <- oneOf validHex
    return c

parseIPv6Hex :: IPv6Parser Word16
parseIPv6Hex = do
    c1 <- parseSingleHex <?> "At least one hex"
    o  <- mapStateT f $ count 3 $ optional parseSingleHex
    let s = c1 : (fromMaybe "" o)
    case readWord16 s of
        Just w  -> return w
        Nothing -> unexpected $ "Invalid piece: " ++ s
    where f :: Parser ([Maybe Char], IPv6State) 
            -> Parser (Maybe [Char], IPv6State)
          f = fmap (\(c,b) -> (sequence c, b))

parseSep :: IPv6Parser String
parseSep = do
    c <- string ":"
    return c

parseCons :: IPv6Parser (Maybe IPv6Piece)
parseCons = do
    b <- get
    c <- optional $ string "::"
    case c of
        Nothing -> return Nothing
        Just _ -> if b
                  then unexpected "Only one '::' allowed"
                  else do
                    put True
                    return (Just $ Left ())

-- Parses: 
--  ::ABCD
--  :ABCD                   
parseInnerParts :: IPv6Parser [IPv6Piece]
parseInnerParts = do
    c <- parseCons
    case c of
        Nothing -> do
            parseSep
            w <- parseIPv6Hex
            return [Right w]
        Just _ -> do
            w <- parseIPv6Hex
            return [Left (), Right w]


parseIPv6Address :: IPv6Parser IPAddress6
parseIPv6Address = do
    c <- parseCons
    w <- parseIPv6Hex
    a <- case c of
            Nothing -> do            
                xs <- mapStateT f $ many parseInnerParts
                c' <- parseCons
                case c' of
                    Nothing -> return ([Right w] ++ xs)
                    Just _  -> return ([Right w] ++ xs ++ [Left ()])
            Just _ -> do
                xs <- mapStateT f $ many parseInnerParts
                return $ ([Left ()] ++ [Right w] ++ xs)
    case getIPAddress6 a of
        Nothing -> unexpected "Invalid format"
        Just a' -> return a'        
    where f :: Parser ([[IPv6Piece]], IPv6State) 
            -> Parser ([IPv6Piece], IPv6State)
          f = fmap (\(xs, b) -> (concat xs, b))

toIPAddress :: IPAddress6 -> Maybe IPAddress
toIPAddress (IPAddress6 w64 w64')
    | w64 /= 0 = Nothing
    | w64' > (fromIntegral $ (maxBound :: Word32)) = Nothing
    | otherwise = Just $ IPAddress (fromIntegral w64')