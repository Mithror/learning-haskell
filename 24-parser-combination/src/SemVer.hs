{-# LANGUAGE OverloadedStrings #-}

module SemVer where

import Data.Char (isDigit)
import Text.Trifecta
import Test.Hspec

data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

-- Probably nicer ways of doing this?
instance Ord SemVer where
    compare (SemVer major minor patch _ _)
            (SemVer major' minor' patch' _ _) =
    --    case compare major major' of
    --     GT -> GT
    --     LT -> LT
    --     EQ ->
    --         case compare minor minor' of
    --             GT -> GT
    --             LT -> LT
    --             EQ ->
    --                 case compare patch patch' of
    --                     GT -> GT
    --                     LT -> LT
    --                     EQ -> EQ 
        mconcat $ 
        zipWith compare [major, minor, patch] [major', minor', patch']

validChars :: [Char]
validChars = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

-- According to SemVer identifiers comprise of [0-9a-zA-Z]
-- Looks like the exercises wants us to differentiate between numbers
-- and strings, even though it can be mixed: e.g 1.0.0-beta+exp.sha.5114f85
-- Therefore we will only treat it as a number if the entire identifier can
-- be identified as a number
parseNumberOrString :: Parser NumberOrString
parseNumberOrString = do
    s <- some $ oneOf validChars
    return $ if all isDigit s
             then NOSI (read s)
             else NOSS s

parserNumberOrStringList :: Parser [NumberOrString]
parserNumberOrStringList = do
    x <- parseNumberOrString
    xs <- many $ char '.' >> parseNumberOrString
    return $ x : xs

parseRelease :: Parser Release
parseRelease = char '-' >> parserNumberOrStringList

parseMetadata :: Parser Metadata
parseMetadata = char '+' >> parserNumberOrStringList

parseSemVer :: Parser SemVer
parseSemVer = do
    major <- decimal
    _ <- char '.'
    minor <- decimal
    _ <- char '.'
    patch <- decimal
    rel <- option [] parseRelease
    metadata <- option [] parseMetadata
    _ <- eof -- not really necessary, but just makes it nice to test :)
    return $ SemVer major minor patch rel metadata

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
    describe "Number or String parsing" $ do
        it "can parse string" $ do
            let m = parseString parseNumberOrString mempty "abc"
                r = maybeSuccess m
            print m
            r `shouldBe` Just (NOSS "abc")
        it "can parse number" $ do
            let m = parseString parseNumberOrString mempty "123"
                r = maybeSuccess m
            print m
            r `shouldBe` Just (NOSI 123)
        it "can parse mixed number and string" $ do
            let m = parseString parseNumberOrString mempty "123abc"
                r = maybeSuccess m
            print m
            r `shouldBe` Just (NOSS "123abc")
        it "fails on non alphanumeric" $ do
            let m = parseString parseNumberOrString mempty "+"
                r = maybeSuccess m
            print m
            r `shouldBe` Nothing

    describe "Number or String parsing" $ do
        it "can parse one number or string" $ do
            let m = parseString parserNumberOrStringList mempty "123abc"
                r = maybeSuccess m
            print m
            r `shouldBe` Just [NOSS "123abc"]
        it "can parse more than one number or string" $ do
            let m = parseString parserNumberOrStringList mempty "123.abc"
                r = maybeSuccess m
            print m
            r `shouldBe` Just [NOSI 123, NOSS "abc"]
        it "fails when no number or string" $ do
            let m = parseString parserNumberOrStringList mempty "+"
                r = maybeSuccess m
            print m
            r `shouldBe` Nothing
    
    describe "Release parsing" $ do
        it "can parse release" $ do
            let m = parseString parseRelease mempty "-123.abc.123abc"
                r = maybeSuccess m
            print m
            r `shouldBe` Just [NOSI 123, NOSS "abc", NOSS "123abc"]
        it "fails when doesn't start with '-'" $ do
            let m = parseString parseRelease mempty "123.abc.123abc"
                r = maybeSuccess m
            print m
            r `shouldBe` Nothing

    describe "Metadata parsing" $ do
        it "can parse metadata" $ do
            let m = parseString parseMetadata mempty "+123.abc.123abc"
                r = maybeSuccess m
            print m
            r `shouldBe` Just [NOSI 123, NOSS "abc", NOSS "123abc"]
        it "fails when doesn't start with '+'" $ do
            let m = parseString parseRelease mempty "123.abc.123abc"
                r = maybeSuccess m
            print m
            r `shouldBe` Nothing
    
    describe "SemVer parsing" $ do
        it "can parse just version" $ do
            let m = parseString parseSemVer mempty "1.2.3"
                r = maybeSuccess m
            print m
            r `shouldBe` Just (SemVer 1 2 3 [] [])
        it "fails on invalid version" $ do
            let m = parseString parseSemVer mempty "1.2"
                r = maybeSuccess m
            print m
            r `shouldBe` Nothing
        it "fails with invalid data after version" $ do
            let m = parseString parseSemVer mempty "1.2.3["
                r = maybeSuccess m
            print m
            r `shouldBe` Nothing
        it "can parse with release" $ do
            let m = parseString parseSemVer mempty "1.2.3-abc"
                r = maybeSuccess m
            print m
            r `shouldBe` Just (SemVer 1 2 3 [NOSS "abc"] [])
        it "fails with more than one release" $ do
            let m = parseString parseSemVer mempty "1.2.3-abc-abc"
                r = maybeSuccess m
            print m
            r `shouldBe` Nothing
        it "fails with invalid data after release" $ do
            let m = parseString parseSemVer mempty "1.2.3-abc]"
                r = maybeSuccess m
            print m
            r `shouldBe` Nothing
        it "can parse with metadata" $ do
            let m = parseString parseSemVer mempty "1.2.3+abc"
                r = maybeSuccess m
            print m
            r `shouldBe` Just (SemVer 1 2 3 [] [NOSS "abc"])
        it "fails with more than one metadata" $ do
            let m = parseString parseSemVer mempty "1.2.3+abc+123"
                r = maybeSuccess m
            print m
            r `shouldBe` Nothing
        it "fails with non number or string after release" $ do
            let m = parseString parseSemVer mempty "1.2.3+abc["
                r = maybeSuccess m
            print m
            r `shouldBe` Nothing
        it "can parse with release and metadata" $ do
            let m = parseString parseSemVer mempty "1.2.3-abc+123"
                r = maybeSuccess m
            print m
            r `shouldBe` Just (SemVer 1 2 3 [NOSS "abc"] [NOSI 123])
    
    describe "Comparing SemVer" $ do
        it "Bigger Major" $ do
            let a = SemVer 2 1 0 [] []
                b = SemVer 1 2 1 [] []
            compare a b `shouldBe` GT
        it "Smaller Major" $ do
            let a = SemVer 1 2 1 [] []
                b = SemVer 2 1 0 [] []
            compare a b `shouldBe` LT
        it "Bigger Minor" $ do
            let a = SemVer 1 2 0 [] []
                b = SemVer 1 1 1 [] []
            compare a b `shouldBe` GT
        it "Smaller Major" $ do
            let a = SemVer 1 1 1 [] []
                b = SemVer 1 2 0 [] []
            compare a b `shouldBe` LT
        it "Bigger Patch" $ do
            let a = SemVer 1 1 2 [] []
                b = SemVer 1 1 1 [] []
            compare a b `shouldBe` GT
        it "Smaller Patch" $ do
            let a = SemVer 1 1 1 [] []
                b = SemVer 1 1 2 [] []
            compare a b `shouldBe` LT
        it "Equal Patch" $ do
            let a = SemVer 1 1 1 [] []
            compare a a `shouldBe` EQ
        it "Release and Major don't matter" $ do
            let a = SemVer 1 1 1 [NOSI 3] [NOSI 3]
                b = SemVer 1 1 1 [NOSI 2, NOSI 1] [NOSI 2, NOSI 1]
            compare a b `shouldBe` EQ