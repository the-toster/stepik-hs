instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Applicative Tree where
    pure a = Branch Nil a Nil
    (Branch lf f rf) <*> (Branch lv x rv) = Branch (lf <*> lv) (f x) (rf <*> lv)
    _ <*> _ = Nil

instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = f x $ foldr f (foldr f ini r) l

instance Traversable Tree where
    traverse f Nil = pure Nil
    traverse f (Branch l x r) = pure Branch <*> (traverse f l) <*> (f x) <*> (traverse f r)
