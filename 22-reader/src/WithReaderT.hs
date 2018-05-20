module WithReaderT where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.State.Class

ctxi :: Reader Integer String
ctxi = do
    i <- ask
    return $ show i

ctxb :: Reader Bool String
ctxb = do
    b <- ask
    return $ show b

trans :: Bool -> Integer
trans True = 1
trans False = 0

myWithReader :: (r' -> r) -> Reader r a -> Reader r' a
myWithReader f m = do
    r' <- ask
    reader $ \r -> runReader m (f r')

trans' :: Integer -> Bool
trans' 0 = False
trans' _ = True

foo :: (r -> r') -> Reader r a -> Reader r' a
foo f m = undefined -- impossible? Let's look at (r -> a)

foo' :: (r -> r') -> (r -> a) -> (r' -> a)
foo' f g = undefined -- impossible...
-- You cannot construct a (r' -> a) from (r -> r') and (r -> a), both functions
-- take an r as input, but we need a function that takes an r' as input.
-- Only way to make this work is by flipping (r -> r'), but then we basically
-- have withReader:
foo'' :: (r' -> r) -> (r -> a) -> (r' -> a)
foo'' f g = g . f

f :: Reader Integer String
f = do
    r <- ask
    -- we cannot change r, but we can do operations on them
    return $ show (r * r)

-- However, we can change the input for f when we call it:
g :: Reader Integer String
g = do
    r <- ask
    -- either via:
    -- return $ runReader f (r + 1)
    -- or:
    withReader (+1) f

main :: IO ()
main = do
    putStrLn $ runReader ctxi 100
    putStrLn $ runReader ctxb False
    putStrLn $ runReader (myWithReader trans ctxi)  True
    putStrLn $ runReader (myWithReader trans ctxi)  False
    putStrLn $ runReader (myWithReader trans' ctxb) 1
    putStrLn $ runReader (myWithReader trans' ctxb) 0