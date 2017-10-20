module TypeKwonDo where

-- 1
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a = (==) (f a)

-- 2
arith :: Num b
      => (a -> b)
      -> Integer
      -> a
      -> b
arith f i a = (f a) + (fromInteger i)