instance Functor Tree where
    fmap f (Leaf a) = Leaf $ fmap f a
    fmap f (Branch l a r) = Branch (fmap f l) (fmap f a) (fmap f r)
