# 23 State

## 23.5 Exercises: Roll Your Own

```haskell
repeat :: a -> [a]
repeat a = a : repeat a

instance Functor (State s) where
fmap f st = state $ \s -> let (a, s') = runState st s
                          in (f a, s')

-- Applying this to infiniteDie means that we advance the StdGen once and
-- repeat the rolled die. I.e. we only call randomR once and then repeat
-- the result.

replicateM :: Applicative f => Int -> f a -> f [a]
replicateM n fa
    | n <= 0 = pure []
    | otherwise = (:) <$> fa <*> replicateM (n - 1) fa

instance Applicative (State s) where
pure a = state $ \s -> (a, s)
stf <*> sta = state $ \s -> let (f, s') = runState stf s
                                (a, s'') = runState sta s'
                            in (f a, s'')

-- randomR will be called for every fmap statement in replicateM. This is
-- because we now from previous statement that fmap will call randomR and the
-- resulting State object will no longer call randomR. The second argument
-- in <*> will eventually reduce to pure [] which won't change the state and
-- call a randomR once for each fmap.
-- Hence we get an array with a call to randomR for each element, propagating
-- the StdGen forward along it.
```

[src/RandomExample2.hs](./src/RandomExample2.hs)

To run in ghci:

```shell
Prelude> :l src/RandomExample2.hs src/RandomExample.hs
```

## 23.6 Write `State` for yourself

[src/Moi.hs](./src/Moi.hs)

## 23.7 Fizzbuzz differently

[src/fizzbuzz.hs](./src/fizzbuzz.hs)

## 23.8 Chapter Exercises

[src/chapter.hs](./src/chapter.hs)