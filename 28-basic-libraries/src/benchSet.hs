module Main where

import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S

bumpIt :: (Num a, Num b) => (a, b) -> (a, b)
bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
    where stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 10000 stream
    where stream = iterate (+1) 0

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

insertMap :: (Int, Int) -> M.Map Int Int
insertMap (k, v) = M.insert k v m

insertSet :: Int -> S.Set Int
insertSet k = S.insert k s

m1 :: M.Map Int Int
m1 = M.fromList $ take 5000 stream
    where stream = iterate (bumpIt . bumpIt) (0, 0)
m2 :: M.Map Int Int
m2 = M.fromList $ take 5000 stream
    where stream = iterate (bumpIt . bumpIt) (1, 1)

s1 :: S.Set Int
s1 = S.fromList $ take 5000 stream
    where stream = iterate (+2) 0
s2 :: S.Set Int
s2 = S.fromList $ take 5000 stream
    where stream = iterate (+2) 1

main :: IO ()
main = defaultMain
    [ bench "member check map" $ whnf (membersMap) 9999
    , bench "member check set" $ whnf (membersSet) 9999
    , bench "insert check set" $ whnf (insertMap) (10001, 10001)
    , bench "insert check set" $ whnf (insertSet) 100001
    , bench "union check set" $ whnf (M.union m1) m2
    , bench "union check set" $ whnf (S.union s1) s2
    ]
