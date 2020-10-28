import Control.Applicative

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
  fmap f (Arr2 g) = Arr2 $ \e1 e2 -> f (g e1 e2)

instance Functor (Arr3 e1 e2 e3) where
  fmap f (Arr3 g) = Arr3 $ \e1 e2 e3 -> f (g e1 e2 e3)

instance Applicative (Arr2 e1 e2) where
  pure a = Arr2 $ \_ _ -> a
  (Arr2 f) <*> (Arr2 g) = Arr2 $ \e1 e2 -> f e1 e2 (g e1 e2)

instance Applicative (Arr3 e1 e2 e3) where
  pure a = Arr3 $ \_ _ _ -> a
  (Arr3 f) <*> (Arr3 g) = Arr3 $ \e1 e2 e3 -> f e1 e2 e3 (g e1 e2 e3)

tst = [
    (getArr2 (Arr2 (\x y z -> x+y-z) <*> Arr2 (*)) 2 3) == -1,
    (getArr3 (Arr3 (\x y z w -> x+y+z-w) <*> Arr3 (\x y z -> x*y*z)) 2 3 4) == -15
    ]
