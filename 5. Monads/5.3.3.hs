instance Functor SomeType where
    fmap f m = m >>= (return . f)
