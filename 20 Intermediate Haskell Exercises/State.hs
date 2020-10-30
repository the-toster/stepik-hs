class Fluffy f where
  furry :: (a -> b) -> f a -> f b

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana (unicorn . f)

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry f (State g) = State (\st -> let (newState, a) = g st in (newState, f a))

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana k (State s) = State $ \st -> let
                            (newStateA, a)  = s st
                            (newStateB, b)  = state (k a) newStateA
                            in (newStateB, b)
  unicorn a = State $ \s -> (s, a)
