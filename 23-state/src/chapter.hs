module Chapter where

newtype State s a = State { runState :: s -> (a, s) }
instance Functor (State s) where
    fmap f k = State $ \s -> let (a, s') = runState k s
                             in (f a, s')
instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    (State f) <*> k = State $ \s -> let (f', s') = f s
                                        (a, s'') = runState k s'
                                    in (f' a, s'')
instance Monad (State s) where
    return = pure
    (State f) >>= k = State $ \s -> let (a, s') = f s
                                    in runState (k a) s'

-- 1
get :: State s s
get = State $ \s -> (s,s)

-- 2
put :: s -> State s ()
put s = State $ \_ -> ((),s)

-- 3
exec :: State s a -> s -> s
exec st = snd . runState st

-- 4
eval :: State s a -> s -> a
eval st = fst . runState st

-- 5
modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)
