import Data.List

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)

instance Foldable Tree where
--  foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = foldr f (f x $ foldr f ini r) l


instance Foldable Preorder where
--  foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f ini (PreO Nil) = ini
    foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini (PreO r)) (PreO l))

instance Foldable Postorder where
--  foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f ini (PostO Nil) = ini
    foldr f ini (PostO (Branch l x r)) = foldr f (foldr f (f x ini) (PostO r)) (PostO l)
--
instance Foldable Levelorder where
--  foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f ini (LevelO tree) = lo [tree] where
        lo [] = ini
        lo (Nil:xs) = lo xs
        lo ((Branch l x r):xs) = f x $ lo $ xs ++ [l, r]

{-
-- на самом деле я решил так. Подсмотрел это ^^^ в решениях и переписал
numerateTree :: Int -> Tree a -> Tree (Int, a)
numerateTree n Nil = Nil
numerateTree n (Branch l x r) = Branch (numerateTree (n + 1) l) (n, x) (numerateTree (n + 2) r)

flatTree :: Tree a -> [a]
flatTree = foldr (:) []

tst = map snd $ sortBy (\n1 n2 -> compare (fst n1) (fst n2)) $ flatTree $ numerateTree 0 tree
-}
