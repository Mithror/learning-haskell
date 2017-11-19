{-# LANGUAGE InstanceSigs #-}
module BiFunctor where

import Data.Bifunctor

data Deux a b = Deux a b
instance Bifunctor Deux where
    bimap :: (a -> b) -> (c -> d) -> Deux a c -> Deux b d
    bimap f g (Deux a b) = Deux (f a) (g b)

data Const a b = Const a
instance Bifunctor Const where
    bimap f _ (Const a) = Const (f a)

data Drei a b c = Drei a b c
instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b
instance Bifunctor (SuperDrei a) where
    bimap f _ (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a
instance Bifunctor (SemiDrei a) where
    bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d
instance Bifunctor (Quadriceps a b) where
    bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data Either' a b = Left' a | Right' b
instance Bifunctor Either' where
    bimap f _ (Left' a) = Left' (f a)
    bimap _ g (Right' b) = Right' (g b)






