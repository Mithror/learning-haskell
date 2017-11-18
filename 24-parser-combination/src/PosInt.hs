{-# LANGUAGE OverloadedStrings #-}

module PosInt where

import Text.Trifecta
import Test.Hspec

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9']

base10Integer :: Parser Integer
base10Integer = do
    xs <- some $ (\a -> read (a:"")) <$> parseDigit
    return $ 
      fst $ foldr (\a (b,i) -> (a * i + b, i*10)) (0,1) xs

parseSign :: Parser Char
parseSign = oneOf ['+', '-']

base10Integer' :: Parser Integer
base10Integer' = do
    c <- option '+' parseSign
    let f =  if c == '-' then negate else id
    f <$> base10Integer

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
    describe "Parse Digit" $ do
        it "can parse a 0" $ do
            let m = parseString parseDigit mempty "0"
                r = maybeSuccess m
            r `shouldBe` Just '0'
        it "can parse a 1" $ do
            let m = parseString parseDigit mempty "1"
                r = maybeSuccess m
            r `shouldBe` Just '1'
        it "can parse a 2" $ do
            let m = parseString parseDigit mempty "2"
                r = maybeSuccess m
            r `shouldBe` Just '2'
        it "can parse a 3" $ do
            let m = parseString parseDigit mempty "3"
                r = maybeSuccess m
            r `shouldBe` Just '3'
        it "can parse a 4" $ do
            let m = parseString parseDigit mempty "4"
                r = maybeSuccess m
            r `shouldBe` Just '4'
        it "can parse a 5" $ do
            let m = parseString parseDigit mempty "5"
                r = maybeSuccess m
            r `shouldBe` Just '5'
        it "can parse a 6" $ do
            let m = parseString parseDigit mempty "6"
                r = maybeSuccess m
            r `shouldBe` Just '6'
        it "can parse a 7" $ do
            let m = parseString parseDigit mempty "7"
                r = maybeSuccess m
            r `shouldBe` Just '7'
        it "can parse a 8" $ do
            let m = parseString parseDigit mempty "8"
                r = maybeSuccess m
            r `shouldBe` Just '8'
        it "can parse a 9" $ do
            let m = parseString parseDigit mempty "9"
                r = maybeSuccess m
            r `shouldBe` Just '9'
        it "other values fail" $ do
            let m = parseString parseDigit mempty "a"
                r = maybeSuccess m
            r `shouldBe` Nothing
    
    describe "Parse Integer" $ do
        it "can parse single digit" $ do
            let m = parseString base10Integer mempty "3"
                r = maybeSuccess m
            r `shouldBe` Just 3
        it "can parse multiple digits" $ do
            let m = parseString base10Integer mempty "123"
                r = maybeSuccess m
            r `shouldBe` Just 123
        it "fails when no digit" $ do
            let m = parseString base10Integer mempty "x23"
                r = maybeSuccess m
            r `shouldBe` Nothing
    
    describe "Parse Sign" $ do
        it "Minus" $ do
            let m = parseString parseSign mempty "-"
                r = maybeSuccess m
            r `shouldBe` Just '-'
        it "Plus" $ do
            let m = parseString parseSign mempty "+"
                r = maybeSuccess m
            r `shouldBe` Just '+'
        it "other" $ do
            let m = parseString parseSign mempty "1"
                r = maybeSuccess m
            r `shouldBe` Nothing
    
    describe "Parse Positive Integer" $ do
        it "positive without +" $ do
            let m = parseString base10Integer' mempty "123abc"
                r = maybeSuccess m
            r `shouldBe` Just 123
        it "positive with -" $ do
            let m = parseString base10Integer' mempty "+123abc"
                r = maybeSuccess m
            r `shouldBe` Just 123
    
    describe "Parse negative integer" $ do
        it "negative" $ do
            let m = parseString base10Integer' mempty "-123abc"
                r = maybeSuccess m
            r `shouldBe` Just (-123)
    