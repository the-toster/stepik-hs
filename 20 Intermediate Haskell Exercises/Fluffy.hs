class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry _ []     = []
  furry f (x:xs) = (f x) : furry f xs

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry _ Nothing = Nothing
  furry f (Just x) = Just $ f x

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry g f = g . f

testMaybeNothing :: Maybe Integer
testMaybeNothing = Nothing

tst = [
        furry id [1,2,3]             == [1,2,3],
        furry ((+1) . (*2)) [1,2,3]  == furry (+1) (furry (*2) [1,2,3]),

        furry id (Just 1)  == Just 1,
        furry id Nothing == testMaybeNothing,
        furry ((+1) . (*2))  (Just 5) == furry (+1) (furry (*2) (Just 5)),

        (furry id (+1)) 1 == (+1) 1,

        furry ((+1) . (*2))  (+1) 4 == furry (+1) (furry (*2) (+1)) 4

     ]

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)


-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry g (EitherLeft (Left a)) = EitherLeft (Left (g a))
  furry g (EitherLeft (Right a)) = EitherLeft (Right a)

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry g (EitherRight (Left a)) = EitherRight (Left a)
  furry g (EitherRight (Right a)) = EitherRight (Right (g a))

