module Main where

import Criterion.Main
import qualified Data.Vector as BV
import qualified Data.Vector.Unboxed as UV

import Control.Monad

total :: Int
total = 10^6

boxed :: BV.Vector Bool
boxed = BV.fromList $ replicate total True

unboxed :: UV.Vector Bool
unboxed = UV.fromList $ replicate total True

main :: IO ()
main =
    defaultMain
    [
        bench "any boxed"   $ whnf (BV.and) boxed,
        bench "any unboxed" $ whnf (UV.and) unboxed
    ]