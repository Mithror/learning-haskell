module Constant where

newtype Constant a b = Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = (Constant a)

instance Monoid a => Applicative (Constant a) where
     pure _ = Constant mempty
     (<*>) (Constant a) (Constant a') = Constant (mappend a a')
