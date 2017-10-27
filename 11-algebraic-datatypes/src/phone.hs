module Phone where
    -- This was quite a doozy.
    -- It's far from perfect, but it does the job.
    import Data.Char
    import Data.List
    
    convo :: [String]
    convo =
        ["Wanna play 20 questions",
         "Ya",
         "U 1st haha",
         "Lol ok. Have u ever tasted alcohol",
         "Lol ya",
         "Wow ur cool haha. Ur turn",
         "Ok. Do u think I am pretty Lol",
         "Lol ya",
         "Just making sure rofl ur turn"]
    
    
    -- validButtons = "1234567890*#"
    type Digit = Char

    -- Valid presses: 1 and up
    type Presses = Int

    contains :: Char -> (Digit, String) -> Bool
    contains c (d,s) = c `elem` d : s

    -- Two types of buttons
    -- A bit silly that you have to now pattern match
    -- on each of them, even though they have the same
    -- 'content'. Better solution? It is a sum of products
    -- rather than a product of sums though...
    data Button = Normal (Digit, String)
                | Capital (Digit, String)
                deriving (Eq, Show)

    -- Some buttons
    isCapital :: Button -> Bool
    isCapital (Capital _) = True
    isCapital _           = False

    isNormal :: Button -> Bool
    isNormal (Normal _) = True
    isNormal _          = False

    contains' :: Char -> Button  -> Bool
    contains' c (Capital a) = contains c a
    contains' c (Normal a)  = contains c a
    
    getDigit :: Button -> Digit
    getDigit (Capital (d,_)) = d
    getDigit (Normal (d, _)) = d

    getPresses :: Char -> (Digit, String) -> Maybe Presses
    getPresses c (d,s) = fmap (+1) $ elemIndex c (d:s) 

    getPresses' :: Char -> Button -> Maybe Presses
    getPresses' c (Capital a) = getPresses c a
    getPresses' c (Normal a)  = getPresses c a 

    -- 1 The phone is just a list of buttons
    data DaPhone = DaPhone [Button] deriving (Eq, Show)

    getCapital :: DaPhone -> Maybe Button
    getCapital (DaPhone xs) = find isCapital xs

    lookUp :: DaPhone -> Char -> Maybe Button
    lookUp (DaPhone xs) c = find (contains' c) xs

    -- A valid phone has one Capital
    -- button
    mkDaPhone :: [Button] -> Maybe DaPhone
    mkDaPhone xs
        | req = Just $ DaPhone xs
        | otherwise = Nothing
        where req = (length $ filter isCapital xs) == 1

    -- 2
    reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
    reverseTaps p c
        | isUpper c = (cap $ getCapital p) ++ (reverseTaps p $ toLower c)
        | otherwise = go m
        where cap Nothing = []
              cap (Just b) = [(getDigit b, 1)]
              m = lookUp p c
              go Nothing = []
              go (Just b) = blah (getPresses' c b) $ (,) $ getDigit b
              blah Nothing _ = [] 
              blah (Just a) f = f a : []

    cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
    cellPhonesDead p = concat . map (reverseTaps p)

    -- 3
    fingerTaps :: [(Digit, Presses)] -> Presses
    fingerTaps = foldr (\(_,p) z -> p + z) 0

    count :: Eq a => a -> [a] -> Int
    count c s = length $ filter (==c) s

    countElements :: Eq a => [a] -> [(a, Int)]
    countElements s = map (\c -> (c,(count c s))) $ nub s
    
    -- 4
    mostPopular :: (Eq a, Ord a) => [a] -> a
    mostPopular s = fst m
        where d = countElements s
              m = maximumBy (\(_,i) (_,j) -> compare i j) d

    myPhone :: Maybe DaPhone
    myPhone = mkDaPhone
        [ Normal ('1', ""),
          Normal ('2', "abc"),
          Normal ('3', "def"),
          Normal ('4', "ghi"),
          Normal ('5', "jkl"),
          Normal ('6', "mno"),
          Normal ('7', "pqrs"),
          Normal ('8', "tuv"),
          Normal ('9', "wvxz"),
          Normal ('0', "+ "),
          Capital ('*', "^"),
          Normal ('#', ".,")]

    -- Applies cellPhonesdead on myPhone
    -- Keep in mind that myPhone is of type Maybe DaPhone
    -- That is why we use fmap to act on the DaPhone
    -- inside the Maybe 'container' (i.e. Functor)
    --
    -- fmap cellPhonesDead myPhone :: Maybe (String -> [(Digit, Presses)])
    -- to which we now want to provide a string, so we need a map that
    -- maps (String -> [(Digit, Presses)]) to [(Digit, Presses)].
    -- In order to do that we want to provide a map that applies a String
    -- to the contained function.
    getDP :: String -> Maybe [(Digit, Presses)]
    getDP s = fmap (\f -> f s) $ fmap cellPhonesDead myPhone

    -- Same deal as getDp, a bit more complex.
    getFT :: String -> Maybe Presses
    getFT s = fmap fingerTaps fil
        where fil = getDP (filter (==(mostPopular s)) s)

    -- 5
    coolestLtr :: [String] -> Char
    coolestLtr = mostPopular . concat
    
    coolestWord :: [String] -> String
    coolestWord = mostPopular

    -- coolestWord :: [String] -> String

    main :: IO ()
    main = do
        print $ map getDP convo
        print $ map mostPopular convo
        print $ map getFT convo
        print $ coolestLtr convo
        print $ coolestWord ((words . concat) convo)