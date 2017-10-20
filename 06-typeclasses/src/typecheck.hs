module TypeCheck where

-- Does it Type Check ?

-- 1 Does not typecheck. Add `deriving Show`
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2 Does not typecheck. Add `deriving Eq`
data Mood = Blah | Woot deriving (Show, Eq)

settleDown :: Mood -> Mood
settleDown x = if x == Woot then Blah else x

-- 3
-- a. Mood values
-- b. Doesn't typecheck as it expects a `Mood` but gets a `Num`
-- c. Won't compile as Mood is not an instance of Ord

-- 4 This typecheckes.
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Show, Eq)

s1 :: Object -> Sentence
s1 = Sentence "dogs" "drool"
s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"

