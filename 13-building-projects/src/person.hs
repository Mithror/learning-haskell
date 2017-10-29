module Person where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Show, Eq)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise =
        Left $ PersonInvalidUnknown $
            "Name was: " ++ show name ++
            " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStr "Provide a name: "
    name <- getLine
    putStr "Provide an age: "
    age <- getLine
    let p = mkPerson name ((read age) :: Integer)
    case go p of
        True  -> putStr "Yay! Succefully got a person: "
        False -> putStrLn "Error: "
    putStrLn $ show p
    where go (Right _) = True
          go (Left _)  = False