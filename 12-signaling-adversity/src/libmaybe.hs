module LibMaybe where

    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    isJust _        = False

    isNothing :: Maybe a -> Bool
    isNothing Nothing = True
    isNothing _       = False

    mayybee :: b -> (a -> b) -> Maybe a -> b
    mayybee b _ Nothing = b
    mayybee _ f (Just a) = f a

    fromMaybe :: a -> Maybe a -> a
    fromMaybe a Nothing = a
    fromMaybe _ (Just a) = a

    listToMaybe :: [a] -> Maybe a
    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x

    maybeToList :: Maybe a -> [a]
    maybeToList Nothing = []
    maybeToList (Just a) = a:[]

    catMaybes :: [Maybe a] -> [a]
    catMaybes [] = []
    catMaybes (Nothing:xs) = catMaybes xs
    catMaybes (Just a:xs) = a : catMaybes xs

    flipMaybe :: [Maybe a] -> Maybe [a]
    flipMaybe = foldr f (Just [])
        where f Nothing _ = Nothing
              f _ Nothing = Nothing
              f (Just a) (Just xs) = Just $ a : xs