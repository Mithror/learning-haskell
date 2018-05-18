module Chapter where

-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- I will implement both foldr and foldMap even though only 1 is necessary

-- 1
data Constant a b = Constant b deriving (Eq, Show)
instance Foldable (Constant a) where
    foldr f z (Constant b) = f b z
    foldMap f (Constant b) = f b
-- 1' Mistake in book as (1) is just Identity with shadow a, probably meant:
data Constant' a b = Constant' a deriving (Eq, Show)
instance Foldable (Constant' a) where
    foldr _ z _ = z
    foldMap _ _ = mempty

-- 2
data Two a b = Two a b deriving (Eq, Show)
instance Foldable (Two a) where
    foldr f z (Two _ b) = f b z
    foldMap f (Two _ b) = f b

-- 3
data Three a b c = Three a b c deriving (Eq, Show)
instance Foldable (Three a b) where
    foldr f z (Three _ _ c) = f c z
    foldMap f (Three _ _ c) = f c

-- 4
data Three' a b = Three' a b b deriving (Eq, Show)
instance Foldable (Three' a) where
    foldr f z (Three' _ b b') = f b $ f b' z
    foldMap f (Three' _ b b') = (f b) `mappend` (f b')

-- 5
data Four' a b = Four' a b b b deriving (Eq, Show)
instance Foldable (Four' a) where
    foldr f z (Four' _ b1 b2 b3) = f b1 $ f b2 $ f b3 z
    foldMap f (Four' _ b1 b2 b3) = (f b1) `mappend` (f b2) `mappend` (f b3)


filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f = foldr (\a b -> if f a then pure a `mappend` b else b) mempty

filterF' :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF' f = foldMap (\a -> if f a then pure a else mempty)