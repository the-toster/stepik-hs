{-# LANGUAGE TypeOperators #-}

infixr 9 |.|
newtype (|.|) f g a = Cmps {getCmps :: f (g a)} deriving (Eq, Show)

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
    foldMap am (Cmps x) = foldMap (\ga -> foldMap am ga) x
tst = [
        (maximum $ Cmps [Nothing, Just 2, Just 3]) == 3,
        (length $ Cmps [[1,2], [], [3,4,5,6,7]]) == 7
    ]
