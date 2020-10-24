import Control.Applicative

data Triple a = Tr a a a  deriving (Eq,Show)

tst = [
        ((^2) <$> Tr 1 (-2) 3)            == Tr 1 4 9,
        (Tr (^2) (+2) (*3) <*> Tr 2 3 4)  == Tr 4 5 12
    ]


instance Functor Triple where
    fmap f (Tr x y z) = Tr (f x) (f y) (f z)


instance Applicative Triple where
    pure a                          = Tr a a a
    (Tr f1 f2 f3) <*> (Tr x y z)    = Tr (f1 x) (f2 y) (f3 z)
