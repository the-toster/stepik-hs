infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand e = if expand1 e == e then e else expand $ expand1 e

expand1 ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
expand1 (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
expand1 (e1 :+: e2) = expand e1 :+: expand e2
expand1 (e1 :*: e2) = expand e1 :*: expand e2
expand1 e = e


tst = expand $ (Val 1 :+: Val 2 :+: Val 3) :*: (Val 4 :+: Val 5)
