module Functions where

-- 1
j :: Monad m => m (m a) -> m a
j x = x >>= id

-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
-- without fmap
l1 f x = do
    a' <- x
    return $ f a'
l1 f x = x >>= (return . f)
-- with fmap :)
-- l1 = fmap

-- 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-- without fmap
l2 f x y = do
    a' <- x
    b  <- y
    return $ f a' b 
-- l2 f x y = x >>= (\a -> y >>= \b -> return $ f a b)
-- with fmap
-- l2 f x y = (fmap f x) <*> y

-- 4
a :: Monad m => m a -> m (a -> b) -> m b
-- without fmap
a x fs = do
    a' <- x
    f  <- fs
    return $ f a'
-- with fmap
-- a x fs = do
--     f <- fs
--     fmap f x
-- a x fs = fs >>= \y -> fmap y x
-- a x fs = fs <*> x

-- 5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = do
    b <- f x
    fmap (b:) $ meh xs f
-- using flipType as base
-- meh as f = flipType (fmap f as)

-- 6
flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id
-- and without using meh
-- flipType [] = return []
-- flipType (x:xs) = do
--     x' <- x
--     fmap (x':) $ flipType xs