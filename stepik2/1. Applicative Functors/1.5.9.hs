{-# LANGUAGE TypeOperators #-}
infixr 9 |.|
newtype (|.|) f g a = Cmps {getCmps :: f (g a)} deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (f |.| g) where
  fmap h (Cmps x) = Cmps $ fmap (fmap h) x

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
  pure = Cmps . pure . pure
  (<*>) = undefined

c = pure 24 :: ([] |.| [] |.| [] |.| []) Int
tst = unCmps4 c

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 x = fmap getCmps $ getCmps x

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 x = fmap (fmap getCmps) $ unCmps3 x
