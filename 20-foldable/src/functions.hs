module Functions where

import Data.Monoid
import Data.Maybe

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
-- -- In order to use foldMap we need a monoid instance for smallest value
-- data Smallest a = Smallest { getSmallest :: Maybe a }
-- instance Ord a => Monoid (Smallest a) where
--     mempty = Smallest Nothing
--     mappend (Smallest Nothing) m = m
--     mappend m (Smallest Nothing) = m
--     mappend (Smallest (Just a1)) (Smallest (Just a2)) =
--         Smallest $ Just (min a1 a2)
-- minimum' = getSmallest . foldMap (Smallest . Just)


maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (\a b ->
                    case b of
                        Nothing -> Just a
                        Just a' -> Just $ max a a')
                 Nothing
-- In order to use foldMap we need a monoid instance for largest value
-- data Largest a = Largest { getLargest :: Maybe a }
-- instance Ord a => Monoid (Largest a) where
--     mempty = Largest Nothing
--     mappend (Largest Nothing) m = m
--     mappend m (Largest Nothing) = m
--     mappend (Largest (Just a1)) (Largest (Just a2)) =
--         Largest $ Just (max a1 a2)
-- maximum' = getLargest . foldMap (Largest . Just)


null' :: Foldable t => t a -> Bool
null' = foldr (\_ _ -> False) True
-- Not very pretty, but it works:
-- null' = isNothing . getFirst . foldMap (First . Just)

length' :: Foldable t => t a ->  Int
length' = foldr (\_ b -> b + 1) 0
-- length' = getSum . foldMap (Sum . const 1)

toList' :: Foldable t => t a -> [a]
toList' = foldr (:) []
-- toList' = foldMap (:[])

fold' :: (Foldable t, Monoid m) => t m -> m
-- fold' = foldMap (mappend mempty) -- mappend mempty = id
fold' = foldMap id
-- fold' = foldr mappend mempty

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- foldMap' f = foldr (\a b -> mappend (f a) b) mempty
foldMap' f = foldr (mappend . f) mempty
