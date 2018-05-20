# Chapter 21 Traversable

## 21.3 sequenceA

```haskell
-- Why: (fmap . fmap) sum Just [1, 2, 3] = Just 6

-- Let's check the types
fmap :: Functor f => (a -> b) -> f a -> f b
(fmap . fmap) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(fmap . fmap) sum :: (Num a, Foldable t, Functor f1, Functor f2) =>
    f1 (f2 (t a)) -> f1 (f2 a)
(fmap . fmap) sum Just :: -- Just :: b -> Maybe b
                          -- Just :: (->) b (Maybe b)
                          -- for this to correspond with f1 (f2 (t a))
                          -- f1 is '(->) b'
                          -- f2 is Maybe
                          -- b is 't a'
                          -- so f1 (f2 a) is (->) (t a) (Maybe a)
                          (Num a, Foldable t) => t a -> Maybe a

-- A good way to see this is that we are lifting the 'sum' over the function
-- functor and the Maybe functor produced as a result of the function functor
--
-- Just :: (->) a (Maybe a)
--
-- And we are lifting (sum :: Foldable t => t a -> a) over (->) a and Maybe.
-- Notice that the a in Just needs to be specialized in a t a to function as
-- the input for sum, i.e
--
-- Just' :: Foldable t => (->) (t a) (Maybe (t a))
--
-- We can now apply (fmap . fmap) sum and get:
-- (fmap . fmap) sum Just :: Foldable t => (->) (t a) (Maybe (t a))

-- It might be helpful to look at the generalized form:
--
-- (fmap . fmap) h1 h2 :: ?
--
(fmap . fmap) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(fmap . fmap) h1 :: (Functor f1, Functor f2) => f1 (f2 a) -> f1 (f2 b)
-- if we now want to add h2, with h2 being: b' -> c', we can deduce that
-- (->) b' is the f1 functor and c' is 'f2 a'
(fmap . fmap) h1 :: Functor f => (b' -> f a) -> (b' -> f b)
-- A couple of points to remember:
--   b' is an input of h2 and a is an input of h1,  so h2 needs to be a function
--   that return returns an input from h1 in a functor (e.g. with Just this can
--   take a 'Foldable t => t a' and returns a Maybe (t a))
-- This implies that there are restrictions imposed on h2:
--    h2 needs to return a functor which contains an input type of h1
-- Applying h2 just gives:
(fmap . fmap) h1 h2 :: (Functor f) => b' -> f b

-- Does this restriction on h2 hold for 'Just'?
-- Consider that sum :: Foldable t => t a -> a
-- And Just :: a -> Maybe a, with a being more generic than 't a', we can
-- indeed say that the resulting a in 'Maybe a' can be of type 't a'.

-- We can rewrite (fmap . fmap) h1 h2 as:
--
-- fmap h1 . h2
--
-- So (fmap . fmap) sum Just can be written as fmap sum . Just

-- Another example with different types accross the board:
foo :: String -> Maybe [Bool]
foo [] = Just []
foo (x:xs) = case foo xs of
    Just xs -> case x of
        't' -> Just $ True : xs
        'f' -> Just $ False : xs
        otherwise -> Nothing
    otherwise -> Nothing

bar :: [Bool] -> Integer
bar = foldr (\b c -> if b then c + 1 else c - 1) 0

-- where (fmap . fmap) bar foo = fmap bar . foo
```

## 21.4 traverse

Why is traverse not defined as:

`traverse :: (Functor f, Traversable t) => (a -> f b) -> t a -> f (t b)`

Maybe let's ask ourselves why sequenceA cannot be defined as such:

`sequenceA :: (Traversable t, Functor f) => t (f a) -> f (t a)`

Let's try to implement it:

```haskell
-- Without applicative, but functor allowed
instance Traversable [] where
    sequenceA [] = ??? -- we have no way of putting this in a functor, we
                       -- would need pure at least...
    sequenceA (x:xs) = ??? -- we have an x :: f a
                           -- and an xs :: [f a]
                           -- We can turn the xs into f [a] via sequenceA:
                           -- sequenceA xs, but now we need to concatenate
                           -- 'f a' and 'f [a]' with only using fmap and this
                           -- is impossible. We can fmap (:) into 'f a', but
                           -- then we need (<*>) to apply this to 'f [a]'

-- with applicative it works:
instance Traversable [] where
    sequenceA [] = pure []
    sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
```

## 21.6 morse code revisited

Why `(sequence .) . fmap` and not `sequence . fmap`

```haskell
fmap :: Functor f => (a -> b) -> (f a -> f b)
sequence :: (Monad m, Traversable t) => t (m a) -> m (t a)
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) sequence :: (Monad m, Traversable t) => (a -> t (m b)) -> (a -> m (t b))
(.) sequence fmap :: -- a :: (a' -> b')
                     -- t (m b) :: (->) (f a') (f b')
                     --    t :: (->) (f a') i.e. (->) (f a') must be Traversable
                     --    m b :: f b' i.e f shoulde be a Monad
                     (Monad m, Traversable (->) (f a)) =>
                     (a -> b) -> m (m a -> b)
-- Which is not really wat we want...
-- However, if you check (.) sequence, then it looks close to what we want
(.) ((.) sequence) :: (Monad m, Traversable t) =>
    (a -> (b -> t (m c))) -> (a -> (b -> m (t c)))
(.) ((.) sequence) fmap :: -- a = (a' -> b')
                           -- b -> t (m c) = (f a') -> f b'
                           -- b = f a'
                           -- t (m c) = f b'
                           --   t = f (A traversable t is also a functor, see
                           --          its definition)
                           --   b' :: m c
                           (Monad m, Traversable t) =>
                           (a' -> m c) -> (t a' -> m (t c))
```

## 21.10 Traversable Laws

```haskell
Compose :: f (g a) -> Compose f g a
f :: Applicative f => a -> f b
g :: Applicative g => a -> f b -- bascause of traverse g
fmap g :: (Applicative f1, Functor f2) => f2 a -> f2 (f1 b)
fmap g . f :: (Applicative f1, Applicative f2) => a -> f2 (f1 b)
Compose . (fmap g . f) :: (Applicative f1, Applicative f2) =>
    a -> Compose f g b
traverse (Compose . fmap g . f) ::
    (Applicative f1, Applicative f2, Traversable t) =>
    t a -> Compose f1 f2 (t b)

traverse f :: (Applicative f, Traversable t) => t a -> f (t b)
fmap (traverse g) :: (Applicative f1, Applicative f2, Traversable t) =>
    f1 (t a) -> f1 (f2 (t b))
fmap (traverse g) . traverse f ::
    (Applicative f1, Applicative f2, Traversable t) =>
    t a -> f1 (f2 (t b))
Compose . fmap (traverse g) . traverse f ::
    (Applicative f1, Applicative f2, Traversable t) =>
    t a -> Compose f1 f2 (t b)
```

## 21.12 Chapter Exercises

see [src/chapter.hs](./src/chapter.hs)