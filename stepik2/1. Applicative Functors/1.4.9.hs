import Control.Applicative

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

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
