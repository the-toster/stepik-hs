newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE p = PrsE f where
  f "" = Left "unexpected end of input"
  f (c:cs) = if p c then Right (c, cs) else Left ("unexpected " ++ [c])

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

tst = [
    (runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "ABC") == Right (('A','B'),"C"),
    (runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "ACD") == Left "unexpected C",
    (runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "BCD") == Left "unexpected B"
    ]

{-
    (charE 'A') >>= (
        \a -> (charE 'B') >>= (
            \b -> return (a, b)
        )
    )
-}

instance Functor PrsE where
  fmap f (PrsE p) = PrsE g where
    g s = do
      (r, s1) <- p s
      return (f r, s1)

instance Applicative PrsE where
  pure a = PrsE (\s -> Right (a, s))
  (PrsE pf) <*> (PrsE pv) = PrsE pr where
    pr s = do
      (f, s1) <- pf s
      (v, s2) <- pv s1
      return (f v, s2)

-- тут тоже должно быть do, ведь выше как-то додумался...
-- но выше это уж месяц назад было наверное.
-- Тогда как-то лучше понимание было
--instance Monad PrsE where
--  (PrsE parser) >>= k = PrsE f where
--        f s = case parser s of
--            (Left e)  -> Left e
--            (Right (a, r)) -> runPrsE (k a) r
instance Monad PrsE where
  (PrsE parser) >>= k = PrsE f where
        f s = do
            (a, r) <- parser s
            runPrsE (k a) r
