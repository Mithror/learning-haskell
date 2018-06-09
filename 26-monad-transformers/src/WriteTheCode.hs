module WriteTheCode where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Identity

import Control.Monad.IO.Class

-- 1
rDec :: Num a => Reader a a
rDec = do
    a <- ask
    return $ a - 1

-- 2
rDec' :: Num a => Reader a a
rDec' = ask >>= return . flip (-) 1

-- 3
rShow :: Show a => ReaderT a Identity String
rShow = do
    a <- ask
    return $ show a

-- 4
rShow' :: Show a => ReaderT a Identity String
rShow' = ask >>= return . show

-- 5
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
    a <- ask
    liftIO $ putStr "Hi: "
    liftIO $ print a
    return $ a + 1

-- 6
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
    a <- get
    liftIO $ putStr "Hi: "
    liftIO $ print a
    put (a + 1)
    return $ show a