import Control.Monad.Reader

--data Reader r a = Reader { runReader :: (r -> a) }
--
--instance Monad (Reader r) where
--  return x = Reader $ \_ -> x
--  m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

newtype State s a = State { runState :: s -> (a,s) }

-- runState :: State s a -> s -> (a,s)
instance Monad (State s) where
    return a = State $ \st -> (a, st)
    m >>= k  = State $ \st ->
        let (a, st') = runState m st
            m' = k a
        in runState m' st'

execState :: State s a -> s -> s
execState m s = snd (runState m s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

readerToState :: Reader r a -> State r a
readerToState m = State $ \r -> (runReader m r, r)

tst = [
        evalState (readerToState $ asks (+2)) 4 == 6,
        runState (readerToState $ asks (+2)) 4  == (6, 4)
    ]

