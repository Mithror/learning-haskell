module Garden where

    data FlowerType = Gardenia
                    | Daisy
                    | Rose
                    | Lilac
                    deriving Show
    
    type Gardener = String

    data Garden = Garden Gardener FlowerType deriving Show

    -- 1
    data Garden' = Gardenia' Gardener
                 | Daisy' Gardener
                 | Rose' Gardener
                 | Lilac' Gardener
                 deriving Show
