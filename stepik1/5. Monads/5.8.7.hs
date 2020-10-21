import Data.Monoid

newtype Writer w a = Writer { runWriter :: (a, w) } deriving Show

writer :: (a, w) -> Writer w a
writer = Writer

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    m >>= k  =
        let (x, u) = runWriter m
            (y, v) = runWriter $ k x
        in Writer (y, u `mappend` v)

tell :: Monoid w => w -> Writer w ()
tell w = writer ((),w)


newtype State s a = State { runState :: s -> (a,s) }
-- runState :: State s a -> s -> (a,s)

instance Monad (State s) where
    return a = State $ \st -> (a, st)
    m >>= k  = State $ \st ->
        let (a, st') = runState m st
            m' = k a
        in runState m' st'

writerToState :: Monoid w => Writer w a -> State w a
writerToState m = State (\st {- hello -} -> ( val, st `mappend` initState ) {- ((), "hello,world") -} ) where
                    (val, initState) = runWriter m

tst = [
        runState (writerToState $ tell "world") "hello," == ((),"hello,world"),
        runState (writerToState $ tell "world") mempty   == ((),"world")
    ]




execState :: State s a -> s -> s
execState m s = snd (runState m s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)
