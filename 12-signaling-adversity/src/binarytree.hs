module BinaryTree where

    data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
                        deriving (Eq, Ord, Show)

    unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
    unfold f a = 
        case (f a) of
            Nothing -> Leaf
            Just (a',b,a'') -> Node (unfold f a') b (unfold f a'')

    treeBuild :: Integer -> BinaryTree Integer
    treeBuild l = unfold f (0 :: Integer)
                    where f n
                            | n < 0  = Nothing
                            | n >= l = Nothing
                            | otherwise = Just (n+1, n, n+1)