
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

tArr = let
                p = unicorn 3 :: (Int -> Int)
                k = (\x y -> x + y) :: (Int -> Int -> Int)
                b = banana k p :: (Int -> Int)
                in  ((-) `banana` ((+) `banana` p)) 1
