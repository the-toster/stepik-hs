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

data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving (Eq, Show)

tst = [
        numberTree (Leaf ()) == Leaf 1,
        numberTree (Fork (Leaf ()) () (Leaf ())) == Fork (Leaf 1) 2 (Leaf 3)
    ]

numberTree :: Tree () -> Tree Integer
numberTree t = evalState (nTree t) 1

nTree :: Tree () -> State Integer (Tree Integer)
nTree (Leaf ()) = do
    n <- get
    put (n + 1)
    return (Leaf n)

nTree (Fork l () r) = do
    ln <- nTree l
    n  <- get
    put (n + 1)
    rn <- nTree r
    return (Fork ln n rn)

-- первая версия, проходит тесты, чтобы проверить, что я правильно понимаю общее решение
--numberTree tree = fst $ nTree 0 tree
--
--nTree n (Leaf ()) = (Leaf (n + 1), n + 1)
--
--nTree n (Fork l () r) = (Fork ln nn rn, rightNumber) where
--    (ln, leftNumber)    = nTree n l
--    nn                  = leftNumber + 1
--    (rn, rightNumber)   = nTree nn r
