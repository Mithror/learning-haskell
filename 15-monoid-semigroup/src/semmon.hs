module SemMon where

import Data.Semigroup (Semigroup, (<>), Sum, Product)
import Data.Monoid (Monoid)
import Test.QuickCheck

-- Note:
-- So as to not have to rewrite all the mappend rules, all Monoid instance,
-- will require the type to also be a Semigroup.

-- Semigroup
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
-- Monoid
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = 
    (a `mappend` (b `mappend` c)) == ((a `mappend` b) `mappend` c)

monoidLeftIdent :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdent a = (mappend mempty a) == a

monoidRightIdent :: (Eq m, Monoid m) => m -> Bool
monoidRightIdent a = (mappend a mempty) == a

-- 1 
-- Semigroup
data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
    _ <> _ = Trivial
instance Arbitrary Trivial where
    arbitrary = return Trivial
type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
-- Monoid
instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)
type TrivId = Trivial -> Bool

-- 2
-- Semigroup
newtype Identity a = Identity a deriving (Eq, Show)
instance Semigroup a => Semigroup (Identity a) where
    (Identity a) <> (Identity a') = Identity $ a <> a'
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a
type IdentityAssoc = Identity String -> 
                     Identity String -> 
                     Identity String -> 
                     Bool
-- Monoid
instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)
type IdentityId = Identity String -> Bool

-- 3
-- Semigroup
data Two a b = Two a b deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b
type TwoAssoc = Two (Sum Int) (Product Int) ->
                Two (Sum Int) (Product Int) ->
                Two (Sum Int) (Product Int) ->
                Bool
-- Monoid
instance (Semigroup a, Semigroup b, Monoid a, Monoid b) 
    => Monoid (Two a b) where
        mempty = Two mempty mempty
        mappend = (<>)
type TwoId = Two (Sum Int) (Product Int) -> Bool 

-- 4
-- Semigroup
data Three a b c = Three a b c deriving (Eq, Show)
instance (Semigroup a, Semigroup b, Semigroup c) 
    => Semigroup (Three a b c) where
        (Three a b c) <> (Three a' b' c') = 
            Three (a <> a') (b <> b') (c <> c')
instance (Arbitrary a, Arbitrary b, Arbitrary c) 
    => Arbitrary (Three a b c) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            return $ Three a b c
type ThreeAssoc = Three (Sum Int) (Product Int) String ->
                  Three (Sum Int) (Product Int) String ->
                  Three (Sum Int) (Product Int) String ->
                  Bool
-- Monoid
instance (Semigroup a, Semigroup b, Semigroup c, Monoid a, Monoid b, Monoid c)
    => Monoid (Three a b c) where
        mempty = Three mempty mempty mempty
        mappend = (<>)
type ThreeId = Three (Sum Int) (Product Int) String -> Bool

-- 5
-- Semigroup
data Four a b c d = Four a b c d deriving (Eq, Show)
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
    => Semigroup (Four a b c d) where
        (Four a b c d) <> (Four a' b' c' d') =
            Four (a <> a') (b <> b') (c <> c') (d <> d')
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => Arbitrary (Four a b c d)  where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            d <- arbitrary
            return $ Four a b c d
type FourAssoc = Four (Sum Int) (Product Int) String Ordering ->
                 Four (Sum Int) (Product Int) String Ordering ->
                 Four (Sum Int) (Product Int) String Ordering ->   
                 Bool
-- Monoid
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d,
          Monoid a, Monoid b, Monoid c, Monoid d) =>
          Monoid (Four a b c d) where
            mempty = Four mempty mempty mempty mempty
            mappend = (<>)
type FourId = Four (Sum Int) (Product Int) String Ordering -> Bool

-- 6
-- Semigroup
newtype BoolConj = BoolConj Bool deriving (Eq, Show)
instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    _ <> _ = BoolConj False
instance Arbitrary BoolConj where
    arbitrary = do
        b <- arbitrary
        return $ BoolConj b
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
-- Monoid
instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)
type BoolConjId = BoolConj -> Bool

-- 7
-- Semigroup
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
instance Semigroup BoolDisj where
    (BoolDisj False) <> (BoolDisj False) = BoolDisj False
    _ <> _ = BoolDisj True
instance Arbitrary BoolDisj where
    arbitrary = do
        b <- arbitrary
        return $ BoolDisj b
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
-- Monoid
instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)
type BoolDisjId = BoolDisj -> Bool

-- 8
-- Semigroup
data Or a b = Fst a | Snd b deriving (Eq, Show)
instance Semigroup (Or a b) where
    Snd a <> _ = Snd a
    _ <> Snd a = Snd a
    _ <> a = a
instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Fst a, Snd b]
type OrAssoc = Or Int Float -> Or Int Float -> Or Int Float -> Bool
-- Monoid 
-- not possible as there is no identity possible.

-- 9
-- Semigroup
newtype Combine a b = Combine { unCombine :: (a -> b)}
-- instance Semigroup b => Semigroup (a -> b) where
--     f <> g = \a -> f a <> g a
-- In other words, f and g are applied to the argument
-- and the results are mappended.
instance Semigroup b => Semigroup (Combine a b) where
    f <> g = Combine $ (unCombine f) <> (unCombine g)
instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = do
        f <- arbitrary
        return $ Combine f
instance Show (Combine a b) where
    show _ = "Combine a b" -- needed for QuickCheck, no idea what to really
                           -- do about this.
semigroupAssoc' :: Int -> -- the input for the function
                   Combine Int String ->
                   Combine Int String ->
                   Combine Int String ->
                   Bool
semigroupAssoc' i f g h = 
    (unCombine (f <> (g <> h))) i == (unCombine ((f <> g) <> h)) i
-- Monoid
instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
    mempty = Combine $ \_ -> mempty -- f a <> mempty a = f a
                                    -- mempty a <> f a = f a
                                    -- f a = b, so mempty is of type b
    mappend = (<>)
monoidAssoc' :: Int -> -- the input for the function
                Combine Int String ->
                Combine Int String ->
                Combine Int String ->
                Bool
monoidAssoc' i f g h = 
    (unCombine (f `mappend` (g `mappend` h))) i 
    == (unCombine ((f `mappend` g) `mappend` h)) i

monoidLeftIdent' :: Int -> Combine Int String -> Bool
monoidLeftIdent' i f = (unCombine (mappend mempty f)) i == (unCombine f) i

monoidRightIdent' :: Int -> Combine Int String -> Bool
monoidRightIdent' i f = (unCombine (mappend f mempty)) i == (unCombine f) i

-- 10
-- Semigroup
newtype Comp a = Comp { unComp :: (a -> a) }
instance Semigroup (Comp a) where
    f <> g = Comp $ unComp f . unComp g
instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
    arbitrary = do
        f <- arbitrary
        return $ Comp f
instance Show (Comp a) where
    show _ = "Comp a" -- needed for QuickCheck, no idea what to really
                      -- do about this.
semigroupAssoc'' :: Int -> -- the input for the function
                   Comp Int ->
                   Comp Int ->
                   Comp Int ->
                   Bool
semigroupAssoc'' i f g h = 
    (unComp (f <> (g <> h))) i == (unComp ((f <> g) <> h)) i
-- Monoid
instance Monoid (Comp a) where
    mempty = Comp id
    mappend= (<>)
monoidAssoc'' :: Int ->
                 Comp Int ->
                 Comp Int ->
                 Comp Int ->
                 Bool
monoidAssoc'' i f g h = 
    (unComp (f `mappend` (g `mappend` h))) i
    == (unComp ((f `mappend` g) `mappend` h)) i

monoidLeftIdent'' :: Int -> Comp Int -> Bool
monoidLeftIdent'' i f = (unComp (mappend mempty f)) i == (unComp f) i

monoidRightIdent'' :: Int -> Comp Int -> Bool
monoidRightIdent'' i f = (unComp (mappend f mempty)) i == (unComp f) i

-- 8 
-- Monoid
newtype Mem s a = Mem { runMem :: s -> (a,s) }
instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
    mappend f g = Mem $ \s ->
        let (a, s')   = runMem g $ s
            (a', s'') = runMem f $ s'
        in (mappend a a', s'')
instance (CoArbitrary s, Arbitrary a, Arbitrary s) => Arbitrary (Mem s a) where
    arbitrary = do
        f <- arbitrary
        return $ Mem f
instance Show (Mem s a) where
    show _ = "Mem s a"
monoidAssoc''' :: Int ->
                  Mem Int String ->
                  Mem Int String ->
                  Mem Int String ->
                  Bool
monoidAssoc''' i f g h =
    (runMem (f `mappend` (g `mappend` h))) i 
    == (runMem ((f `mappend` g) `mappend` h)) i

monoidLeftIdent''' :: Int -> Mem Int String -> Bool
monoidLeftIdent''' i f = (runMem (mappend mempty f)) i == (runMem f) i

monoidRightIdent''' :: Int -> Mem Int String -> Bool
monoidRightIdent''' i  f = (runMem (mappend f mempty)) i == (runMem f) i

main :: IO ()
main = do
    putStrLn "Semigroup tests"
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (semigroupAssoc :: IdentityAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: ThreeAssoc)
    quickCheck (semigroupAssoc :: FourAssoc)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (semigroupAssoc :: OrAssoc)
    quickCheck semigroupAssoc'
    quickCheck semigroupAssoc''
    
    putStrLn "Monoid tests - Associativity" -- actually same as semigroup
    quickCheck (monoidAssoc :: TrivAssoc)
    quickCheck (monoidAssoc :: IdentityAssoc)
    quickCheck (monoidAssoc :: TwoAssoc)
    quickCheck (monoidAssoc :: ThreeAssoc)
    quickCheck (monoidAssoc :: FourAssoc)
    quickCheck (monoidAssoc :: BoolConjAssoc)
    quickCheck (monoidAssoc :: BoolDisjAssoc)
    quickCheck monoidAssoc'
    quickCheck monoidAssoc''
    quickCheck monoidAssoc'''

    putStrLn "Monoid tests - Left Identity"
    quickCheck (monoidLeftIdent :: TrivId)
    quickCheck (monoidLeftIdent :: IdentityId)
    quickCheck (monoidLeftIdent :: TwoId)
    quickCheck (monoidLeftIdent :: ThreeId)
    quickCheck (monoidLeftIdent :: FourId)
    quickCheck (monoidLeftIdent :: BoolConjId)
    quickCheck (monoidLeftIdent :: BoolDisjId)
    quickCheck monoidLeftIdent'
    quickCheck monoidLeftIdent''
    quickCheck monoidLeftIdent'''

    putStrLn "Monoid tests - Right Identity"
    quickCheck (monoidRightIdent :: TrivId)
    quickCheck (monoidRightIdent :: IdentityId)
    quickCheck (monoidRightIdent :: TwoId)
    quickCheck (monoidRightIdent :: ThreeId)
    quickCheck (monoidRightIdent :: FourId)
    quickCheck (monoidRightIdent :: BoolConjId)
    quickCheck (monoidRightIdent :: BoolDisjId)
    quickCheck monoidRightIdent'
    quickCheck monoidRightIdent''
    quickCheck monoidRightIdent'''