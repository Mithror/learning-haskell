module Hypo where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

-- r -> Maybe a
foo :: Int -> ReaderT Int Maybe Int
foo a = return a


-- (Reader r) (Maybe a)
-- r -> Maybe a
bar :: Int -> MaybeT (Reader Int) Int
bar a = return a

-- looks pretty much the same to me...
main :: IO ()
main = do
    print $ runReaderT (foo 1) 5
    print $ runReader (runMaybeT (bar 1)) 5