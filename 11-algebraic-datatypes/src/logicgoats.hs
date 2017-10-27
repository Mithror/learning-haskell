{-# LANGUAGE FlexibleInstances #-}

module LogicGoats where

    class TooMany a where
        tooMany :: a -> Bool

    -- 1
    newtype IntString = IntString (Int,String)    
    instance TooMany IntString where
        tooMany (IntString (n, _)) = n > 42
    
    -- needs FlexibleInstances
    instance TooMany (Int, String) where
        tooMany (n, _) = n > 42

    -- 2
    newtype Goats = Goats (Int, Int)
    instance TooMany Goats where
        tooMany (Goats (m, n)) = (m+n)>42

    --3
    -- needs FlexibleInstances
    instance (Num a, TooMany a) => TooMany (a,a) where
        tooMany (a,b) = tooMany $ a + b