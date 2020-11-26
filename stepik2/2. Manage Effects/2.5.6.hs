import Control.Applicative

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE p = PrsE f where
  f "" = Left "unexpected end of input"
  f (c:cs) = if p c then Right (c, cs) else Left ("unexpected " ++ [c])

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

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

instance Alternative PrsE where
  empty = PrsE f where
    f _ = Left "empty alternative"
  p <|> q = PrsE f where
    f s = let ps = runPrsE p s
      in if null ps
         then runPrsE q s
         else ps
