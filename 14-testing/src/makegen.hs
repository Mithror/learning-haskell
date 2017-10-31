module MakeGen where

import Test.QuickCheck

-- 1
data Fool = Fulse | Frue deriving (Show, Eq)

equal_foolGen :: Gen Fool
equal_foolGen = elements [Fulse, Frue]

-- 2
skewed_foolGen :: Gen Fool
skewed_foolGen = frequency [ (2, return Fulse), (1, return Frue) ]

