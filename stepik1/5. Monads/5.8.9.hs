import Control.Monad (replicateM_)

newtype State s a = State { runState :: s -> (a,s) }
-- runState :: State s a -> s -> (a,s)

instance Monad (State s) where
    return a = State $ \st -> (a, st)
    m >>= k  = State $ \st ->
        let (a, st') = runState m st
            m' = k a
        in runState m' st'

get :: State s s
get = State $ \st -> (st, st)

put :: s -> State s ()
put st = State $ \_ -> ((), st)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)


fibStep :: State (Integer, Integer) ()
fibStep = do
    (a, b) <- get
    put (b, a + b)

execStateN :: Int -> State s a -> s -> s
execStateN n m =  execState $ replicateM_ n m

fib n = fst $ execStateN n fibStep (0, 1)

tst = [
    execState fibStep (0,1) == (1,1),
    execState fibStep (1,1) == (1,2),
    execState fibStep (1,2) == (2,3),
    fib 10 == 55
    ]
