module Sing where

-- ++ should be ->
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

-- Char should be [Char]
sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

-- added type signature
-- y should be 'Somewhere'
sing :: [Char]
sing = if (x > y) then fstString x else sndString  y
  where x = "Singin"
        y = "Somewhere"

-- 2
singOther :: [Char]
singOther = if (x <= y) then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"