module Mood where

data Mood = Woot | Blah deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah