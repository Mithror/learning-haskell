module TypeKwonDo where

-- 1
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h = g . f

-- 2
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

-- 3
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- 4
munge :: (x -> y) 
      -> (y -> (w, z))
      -> x
      -> w
munge f1 f2 = fst . f2 . f1
--munge f1 f2 x = fst $ (f2 . f1) x
-- (f2 . f1) :: x -> (w,z)
-- munge f1 f2 = fst $ (f2 . f1) 
-- doesn't work because you apply fst to a function and it 
-- expects a tuple. (f2 . f1) needs to be applied to an argument to get the
-- tuple on which fst can operate.