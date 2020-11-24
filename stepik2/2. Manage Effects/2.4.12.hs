{-# OPTIONS_GHC -Wincomplete-patterns #-}

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un a)         = a
concatOC (Bi a b oddC)  = concat3OC a b (concatOC oddC)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a) (Un b) c            = Bi a b c
concat3OC (Un a) (Bi b c odd1) odd2  = concat3OC (Un a) (Un b) (concat3OC (Un c) odd1 odd2)
concat3OC (Bi a b odd1) odd2 odd3    = concat3OC (Un a) (Un b) (concat3OC odd1 odd2 odd3)

tst1 = Bi 10 20 (Un 30)
tst2 = Bi 1 2 (Bi 3 4 (Un 5))
tst = [
        (do {x <- tst1; y <- tst2; return (x + y)}) == (Bi 11 12 (Bi 13 14 (Bi 15 21 (Bi 22 23 (Bi 24 25 (Bi 31 32 (Bi 33 34 (Un 35)))))))),
        (do {x <- tst2; y <- tst1; return (x + y)}) == (Bi 11 21 (Bi 31 12 (Bi 22 32 (Bi 13 23 (Bi 33 14 (Bi 24 34 (Bi 15 25 (Un 35))))))))
    ]

instance Functor OddC where
    fmap f (Un a) = Un $ f a
    fmap f (Bi a b c) = Bi (f a) (f b) (fmap f c)

instance Applicative OddC where
    pure = Un
    (Un f) <*> (Un v) = Un $ f v
    (Un f) <*> (Bi v1 v2 oddV) = Bi (f v1) (f v2) ((Un f) <*> oddV)
    (Bi f1 f2 oddF) <*> oddV = concat3OC f1r f2r rest where
        f1r  = (Un f1) <*> oddV
        f2r  = (Un f2) <*> oddV
        rest = oddF <*> oddV

instance Monad OddC where
    oddC >>= k = concatOC $ fmap k oddC

