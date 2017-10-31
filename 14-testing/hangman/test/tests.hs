module Main where

    import Hangman
    import Test.Hspec        

    testPuzzle :: Puzzle
    testPuzzle = Puzzle "test" (take 4 $ repeat Nothing) "" 0
    
    correctChar :: Char
    correctChar = 't'

    guessCorrect :: Puzzle
    guessCorrect = 
        Puzzle "test" 
               [Just correctChar, Nothing, Nothing, Just correctChar]
               [correctChar]
               0

    wrongChar :: Char
    wrongChar = 'a'

    guessWrong :: Puzzle
    guessWrong = 
        Puzzle "test"
               [Nothing, Nothing, Nothing, Nothing] 
               [wrongChar] 
               0
    
    test_fillInCharacter :: IO ()
    test_fillInCharacter = hspec $ do
        describe "fillInCharacter" $ do
            it "Wrong guess only adds to guesses" $ do
                fillInCharacter testPuzzle wrongChar 
                == guessWrong
            it "Right guess updates guesses and characters guessed so far" $ do
                fillInCharacter testPuzzle correctChar
                == guessCorrect

    -- I feel like I don't grasp enough of the language yet
    -- to propery do this. I can't compare IO Puzzle with Puzzle
    -- and using <- to bind the Puzzle doesn't work yet.
    -- test_handleGuess :: IO ()
    -- test_handleGuess = hspec $ do
    --     describe "handleGuess" $ do
    --         it "Wrong guess only adds to guess" $ do
    --             p <- handleGuess testPuzzle wrongChar
    --             p == guessWrong
                        
    main :: IO ()
    main = do
        return ()
        test_fillInCharacter
        -- test_handleGuess