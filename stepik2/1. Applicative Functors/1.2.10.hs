import Control.Applicative

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
  fmap f arr2 = \e1 e2 -> Arr2 e1 e2 f $ getArr2 arr2 e1 e2

instance Functor (Arr3 e1 e2 e3) where
  fmap f arr3 = \e1 e2 e3 -> Arr2 e1 e2 e3 f $ getArr3 arr3 e1 e2 e3

instance Applicative (Arr2 e1 e2) where
  pure = undefined
  (<*>) = undefined

instance Applicative (Arr3 e1 e2 e3) where
  pure = undefined
  (<*>) = undefined

tst = [
    (getArr2 (Arr2 (\x y z -> x+y-z) <*> Arr2 (*)) 2 3) == -1,
    (getArr3 (Arr3 (\x y z w -> x+y+z-w) <*> Arr3 (\x y z -> x*y*z)) 2 3 4) == -15
    ]
