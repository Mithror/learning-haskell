{-# LANGUAGE InstanceSigs #-}
module Gotcha where

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity (f a)

--
newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

myApply :: (Applicative f, Applicative g)
        => Compose f g (a -> b) -> Compose f g a -> Compose f g b
myApply (Compose h) (Compose c) =
    Compose $ (fmap (<*>) h) <*> c

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose $ pure (pure a)
    -- The next one took me a while. I find the following usefull in
    -- understanding the solution. Ignoring the Compose part of it we
    -- have a type signature of:
    -- :: f (g (a -> b)) -> f (g a) -> f (g b)
    -- In the applicative the following would be easy:
    -- :: f (g a -> g b) -> f (g a) -> f (g b)
    -- as that would just (<*>), if we somehow find a way to go from
    -- :: f (g (a -> b)) to (f (g a -> g b)) using applicative and/or functor
    -- functions we would be set.
    -- If you ignore the f part of it,  you see we have:
    -- :: g (a -> b) -> g a -> g b which is nothing but (<*>)
    -- We can also see this as:
    -- :: g (a -> b) -> (g a -> g b)
    -- In other words this is a function that returns another function where
    -- we know that the first argument/function is embeded in an f. We can use
    -- the fact that f is a functor to actually apply the function:
    -- :: fmap (g (a -> b) -> (g a -> g b)) (f (g (a -> b))) :: f (g a -> g b)
    -- Which brings us to:
    -- :: (<*>) f a = (fmap (<*>) f) <*> a
    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose h) <*> (Compose c) = Compose $
        (fmap (<*>) h) <*> c

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap :: (Monoid m) => (a -> m) -> Compose f g a -> m
    foldMap f (Compose fga) = (foldMap . foldMap) f fga

-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> (t a -> m)
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- Foldmap for f :: (g a -> m) -> f (g a) -> m
-- FoldMap for g :: (a -> m) -> g a -> m
-- (foldMap . foldMap) :: (a -> m) -> f (g a) -> m

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse :: Applicative f' => (a -> f' b) -> Compose f g a
                                            -> f' (Compose f g b)
    traverse f (Compose fga) = Compose <$> ((traverse . traverse) f fga)

-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
-- traverse for f :: ((g a) -> f' (g b)) -> f (g a) -> f' (f (g b))
-- traverse for g :: (a -> f' b) -> g a -> f' (g b)
-- (traverse . traverse) :: (a -> f' b) -> f (g a) -> f' (f g b)



