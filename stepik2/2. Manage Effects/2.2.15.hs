

instance (Traversable f, Traversable g) => Traversable (f |.| g) where
    traverse h (Cmps x) = pure Cmps <*> traverse (traverse h) x
