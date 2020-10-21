data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving (Eq, Show)

tst = [
        numberTree (Leaf ()) == Leaf 1,
        numberTree (Fork (Leaf ()) () (Leaf ())) == Fork (Leaf 1) 2 (Leaf 3)
    ]

numberTree :: Tree () -> Tree Integer
numberTree tree = undefined


-- первая версия, проходит тесты, чтобы проверить, что я правильно понимаю общее решение
--numberTree tree = fst $ nTree 0 tree
--
--nTree n (Leaf ()) = (Leaf (n + 1), n + 1)
--
--nTree n (Fork l () r) = (Fork ln nn rn, rightNumber) where
--    (ln, leftNumber)    = nTree n l
--    nn                  = leftNumber + 1
--    (rn, rightNumber)   = nTree nn r
