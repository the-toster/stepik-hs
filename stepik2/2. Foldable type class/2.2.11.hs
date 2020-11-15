data Triple a = Tr a a a  deriving (Eq,Show)


instance Functor Triple where
    fmap f (Tr x y z) = Tr (f x) (f y) (f z)


instance Applicative Triple where
    pure a                          = Tr a a a
    (Tr f1 f2 f3) <*> (Tr x y z)    = Tr (f1 x) (f2 y) (f3 z)

instance Foldable Triple where
    foldr f ini (Tr a b c) = foldr f ini [a, b, c]
    foldl f ini (Tr a b c) = foldl f ini [a, b, c]


instance Traversable Triple where
--      traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
      traverse f (Tr a b c) = pure Tr <*> f a  <*> f b <*> f c
--      traverse f x = pure Tr
