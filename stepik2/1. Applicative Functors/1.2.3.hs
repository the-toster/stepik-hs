import Control.Applicative

purew x = [x,x]
xs = [2,4]
fs = [(^2)]
x = 2
g = (^3)
us = [(*3)]
vs = [(+3)]
tst = [
        (purew id <*> xs) == xs, --  Identity: False                                         - check
        (purew (.) <*> us <*> vs <*> xs) == (us <*> (vs <*> xs)), -- Composition: False      - check
        (fs <*> purew x) == (purew ($ x) <*> fs), --  Interchange: True                      - non check
        (g <$> xs) == (purew g <*> xs) --  Applicative-Functor: True                         - non check
        (purew g <*> purew x) == purew (g x), -- Homomorphism: False                         - check
    ]
