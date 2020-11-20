import Data.Traversable (foldMapDefault)

data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)

instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
    foldMap = foldMapDefault

instance Traversable Tree where
    sequenceA Nil = pure Nil
    sequenceA (Branch l x r) =
        (pure Branch) <*>
        (sequenceA l) <*> x <*> (sequenceA r)
