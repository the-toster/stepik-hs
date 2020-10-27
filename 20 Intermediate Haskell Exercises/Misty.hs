
class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana (unicorn . f)

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana k a = concatMap k a
  unicorn = (:[])


-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana k Nothing = Nothing
  banana k (Just a) = k a
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
-- banana :: (a -> m b) -> m a -> m b
-- banana :: (a -> t -> b) -> (t -> a) -> (t -> b)
  banana f tToa = \t -> f (tToa t) t
  unicorn e = \x -> e

testArrLaws = [testLaw1, testLaw2, testLaw3] where
        bna :: Misty m => m a -> (a -> m b) -> m b
        bna = flip banana
        returnA :: (Integer -> Integer)
        a = 2 :: Integer
        returnA = unicorn a
        f = (^)
        m = f a
        g = (+)
        testLaw1 = (returnA `bna` f) 10      == (f a) 10
        testLaw2 = (m `bna` unicorn) 10      == m 10
        testLaw3 = ((m `bna` f) `bna` g) 10  == (m `bna` (\x -> f x `bna` g)) 10



newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
--  banana :: (a -> m b) -> m a -> m b
--  banana :: (a -> EitherLeft t b) -> EitherLeft t a -> EitherLeft t b
  banana k (EitherLeft (Left a)) = k a
  banana _ (EitherLeft (Right a)) = EitherLeft (Right a)
--  unicorn :: a -> m a
  unicorn b = EitherLeft (Left b)


-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana k (EitherRight (Right a)) = k a
  banana _ (EitherRight (Left a)) = EitherRight (Left a)
  unicorn a = EitherRight (Right a)


-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id


-- Exercise 13
-- Relative Difficulty: 6
--apple :: (Misty m) => m a -> m (a -> b) -> m b
--apple ma mab = jellybean $ banana (\a -> (banana (\fab -> fab a)  mab)) ma
apl :: (Misty m) => m a -> m (a -> b) -> m b
apl ma mab = banana (\fab -> fab a)
--
--  banana :: (a -> m b) -> m a -> m b
--  unicorn :: a -> m a
--  furry' :: (a -> b) -> m a -> m b
