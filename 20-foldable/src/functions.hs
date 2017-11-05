module Functions where

import Data.Monoid

-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

sum' :: (Foldable t, Num a) => t a -> a
-- sum' = foldr (+) 0
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
-- product' = foldr (*) 1
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
-- elem' e = foldr (\a b -> a == e || b) False
elem' e = getAny . (foldMap (Any . (==e)))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr (\a b -> 
                    case b of
                        Nothing -> Just a
                        Just a' -> Just $ min a a')
                 Nothing

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (\a b ->
                    case b of
                        Nothing -> Just a
                        Just a' -> Just $ max a a')
                 Nothing

null' :: Foldable t => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: Foldable t => t a ->  Int
length' = foldr (\_ b -> b + 1) 0

toList' :: Foldable t => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
-- fold' = foldMap (mappend mempty) -- mappend mempty = id
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- foldMap' f = foldr (\a b -> mappend (f a) b) mempty
foldMap' f = foldr (mappend . f) mempty