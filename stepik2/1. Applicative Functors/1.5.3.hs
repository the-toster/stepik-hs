{-# LANGUAGE TypeOperators #-}
newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Eq,Show)
infixr 9 |.|

type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (1, ('a', True))
b :: B t
b = Cmps (True, id, Right 1)

c :: C
c  = Cmps $ \_ _ -> 1
