
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
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple ma mab = banana (\ab -> banana (\a -> unicorn (ab a) ) ma) mab
--  banana :: (a -> m b) -> m a -> m b
--  unicorn :: a -> m a
--  furry' :: (a -> b) -> m a -> m b

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy xs k = foldl (\acc mb -> banana (\b -> banana (\bs -> unicorn (b:bs) ) acc) mb) (unicorn []) (map k xs)

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage ms = moppy ms id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
-- apple :: (Misty m) => m a -> m (a -> b) -> m b
--  furry' :: (a -> b) -> m a -> m b
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f ma mb = apple mb (furry' f ma)

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f ma mb mc = apple mc (banana2 f ma mb)

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f ma mb mc md = apple md (banana3 f ma mb mc)
