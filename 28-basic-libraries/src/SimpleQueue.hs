module Main where

import Criterion.Main
import qualified Data.Sequence as S

data Queue a = Queue { enqueue :: [a]
                     , dequeue :: [a]
                     } deriving (Show, Eq)

empty :: Queue a
empty = Queue [] []

push :: a -> Queue a -> Queue a
push a q = q { enqueue = a : enqueue q}

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue xs []) = pop $ Queue [] (reverse xs)
pop (Queue xs (y:ys)) = Just (y, Queue xs ys)

alternateQ :: [Int] -> Bool
alternateQ [] = True
alternateQ (x:xs) = go x empty && alternateQ xs
    where go y q = f (pop $ push y q) y
          f Nothing _ = False
          f (Just (a, _)) b = a == b

pushL :: a -> [a] -> [a]
pushL = (:)

popL :: [a] -> Maybe (a, [a])
popL [] = Nothing
popL xs = let xs' = reverse xs
          in Just (head $ xs', reverse $ tail xs')

alternateL :: [Int] -> Bool
alternateL [] = True
alternateL (x:xs) = go x [] && alternateL xs
    where go y q = f (popL $ pushL y q) y
          f Nothing _ = False
          f (Just (a, _)) b = a == b

pushThenPop :: [Int] -> [Int]
pushThenPop xs = (go' $ go xs empty)
    where go [] q = q
          go (y:ys) q = go ys (push y q)
          go' q = let r = pop q
                  in case r of
                    Nothing -> []
                    (Just (a, q')) -> a : go' q'

pushThenPopL :: [Int] -> [Int]
pushThenPopL xs = (go' $ go xs [])
    where go [] q = q
          go (y:ys) q = go ys (pushL y q)
          go' q = let r = popL q
                  in case r of
                    Nothing -> []
                    (Just (a, q')) -> a : go' q'

--
fromList :: [a] -> Queue a
fromList = flip Queue []

fromList' :: [a] -> Queue a
fromList' = Queue [] . reverse

(><) :: Queue a -> Queue a -> Queue a
(><) (Queue e d) (Queue e' d') = Queue [] q''
    where q'' = d ++ reverse e ++ d' ++ reverse e'

testConcat :: [Int] -> Queue Int
testConcat xs = fromList xs >< fromList xs

testConcat' :: [Int] -> S.Seq Int
testConcat' xs = S.fromList xs S.>< S.fromList xs

pushA :: [Int] -> Queue Int
pushA xs = go xs empty
    where go [] q = q
          go (x:xs) q = go xs $ push x q

pushA' :: [Int] -> S.Seq Int
pushA' xs = go xs S.empty
    where go [] s = s
          go (x:xs) s = go xs $ x S.<| s

main :: IO ()
main = defaultMain
    -- List will be faster because it has no overehead for Queue construction
    -- and queue still needs to reverse for every element just as list.
    [ bench "queue" $ nf alternateQ [1..1000]
    , bench "list" $ nf alternateL [1..1000]
    -- List will be slower than queue, because it needs to reverse for every
    -- element, while queue only reverse on empty dequeue.
    , bench "queue push pop" $ nf pushThenPop [1..1000]
    , bench "list push pop" $ nf pushThenPopL [1..1000]
    -- Queue is quite efficient at what it is good at:
    , bench "queue fromList" $ whnf fromList ([1..1000] :: [Int])
    , bench "queue fromList'" $ whnf fromList' ([1..1000] :: [Int])
    , bench "sequence fromList" $ whnf S.fromList ([1..1000] :: [Int])
    , bench "queue concat" $ whnf testConcat ([1..100] :: [Int])
    , bench "sequence concat" $ whnf testConcat' ([1..100] :: [Int])
    , bench "insert queue" $ whnf pushA [1..1000]
    , bench "insert sequence" $ whnf pushA' [1..1000]
    ]