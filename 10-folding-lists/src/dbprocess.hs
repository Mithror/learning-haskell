module DBProcess where

    import Data.Time

    data DatabaseItem = DbString String
                      | DbNumber Integer
                      | DbDate   UTCTime
                      deriving (Eq, Ord, Show)

    theDatabase :: [DatabaseItem]
    theDatabase =
        [ DbDate (UTCTime 
                  (fromGregorian 1911 5 1)
                  (secondsToDiffTime 34123))
        , DbNumber 9001
        , DbString "Hello, world!"
        , DbDate (UTCTime
                  (fromGregorian 1921 5 1)
                  (secondsToDiffTime 34123))
        ]

    -- 1
    filterDbDate :: [DatabaseItem] -> [UTCTime]
    filterDbDate = foldr f []
        where f (DbDate u) xs = u : xs
              f _          xs = xs

    -- 2
    filterDbNumber :: [DatabaseItem] -> [Integer]
    filterDbNumber = foldr f []
        where f (DbNumber i) xs = i : xs
              f _            xs = xs
    
    -- 3
    mostRecent :: [DatabaseItem] -> UTCTime
    mostRecent = maximum . filterDbDate

    -- with folds, this is more difficult, because it's not easy to find the
    -- right zero value. Ideally it would be the smallest UTCTime possible.
    mostRecent' :: [DatabaseItem] -> UTCTime
    mostRecent' = foldr f z
        where f (DbDate u1) u2 = max u1 u2
              f _           u  = u
              z                = UTCTime d 0
              d = (toEnum (minBound :: Int) :: Day)
    -- The problem here is that even this day is not the smallest possible, 
    -- because you can simply do `pred d` which gives a smaller day. So the
    -- original solution is superior.

    -- 4 
    sumDb :: [DatabaseItem] -> Integer
    sumDb = sum . filterDbNumber

    -- with folds
    sumDb' :: [DatabaseItem] -> Integer
    sumDb' = foldr f 0
        where f (DbNumber i1) i2 = i1 + i2
              f _           i2   = i2

    -- 5
    avgDb :: [DatabaseItem] -> Double
    avgDb [] = undefined -- 0/0
    avgDb xs = (fromIntegral . sumDb $ xs ) / (fromIntegral . length $ xs)