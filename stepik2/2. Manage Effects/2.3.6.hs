data OddC a = Un a | Bi a a (OddC a)

instance Functor OddC where
    fmap f (Un a) = Un $ f a
    fmap f (Bi a b c) = Bi (f a) (f b) (fmap f c)

instance Foldable OddC where
    foldr f ini (Un a) = f a ini
    foldr f ini (Bi a b c) = f a $ f b $ foldr f ini c

instance Traversable OddC where
    traverse f (Un a) = pure Un <*> f a
    traverse f (Bi a b c) = pure Bi <*> (f a) <*> (f b) <*> traverse f c
