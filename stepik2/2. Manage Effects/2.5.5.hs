import Control.Monad

-- контрпример
k :: Int -> Maybe Int
k = \x -> if x == 2 then Nothing else Just 2
u = Just 2
v = Just 1

tst = [
    ((u `mplus` v) >>= k) == ((u >>= k) `mplus` (v >>= k))
  ]


-- Докажем, что для Maybe и стандартной реализации,
-- закон ((u <|> v) <*> w) == (u <*> w <|> v <*> w)
-- выполняется
-- Запишем  стандартные реализации:
-- fmap _ Nothing   = Nothing
-- fmap f (Just a)  = Just (f a)
-- Just f  <*> m    = fmap f m
-- Nothing <*> _m   = Nothing
-- Nothing <|> r    = r
-- l       <|> _    = l

-- закон (u <|> v) <*> w == u <*> w <|> v <*> w
-- подставим определения в левую часть, используя все
-- для случая если u, v, w построены конструктором Just
-- (заменим u на Just u и т.д. для наглядности преобразований)
(Just u <|> Just v) <*> Just w == -- <|>
Just u <*> Just w              == -- <*>
fmap u (Just w)                == -- fmap
Just (u w)

--  и в правую
Just u <*> Just w <|> Just v <*> Just w == -- <*>
fmap u (Just w) <|> fmap v (Just w)     == -- fmap
Just (u w) <|> Just (u v)               == -- <|>
Just (u w)
-- обе части равны


-- рассмотрим варианты, если хотя бы одна из переменных = Nothing
-- для случая когда u = Nothing
-- левая
(Nothing <|> v) <*> w == -- <|>
v <*> w
-- правая
(Nothing <*> w) <|> (v <*> w) == -- <*>
Nothing <|> (v <*> w)         == -- <|>
v <*> w

-- для случая когда v = Nothing
-- левая
(u <|> Nothing) <*> w == -- <|>
u <*> Just w
-- правая
u <*> w <|> Nothing <*> w == -- <*>
u <*> w <|> Nothing       == -- <|>
u <*> w
-- обе части равны

-- для случая w = Nothing
-- левая
(u <|> v) <*> Nothing == -- <*>
Nothing

-- правая
u <*> Nothing <|> v <*> Nothing == -- <*>
Nothing <|> Nothing             == -- <|>
Nothing
-- обе части равны

-- т.о. мы перебрали все возможные варианты стандартных определений.
-- Возможно это что-то доказывает...
