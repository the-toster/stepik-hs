import Control.Applicative

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }


instance Functor Prs where
    fmap f parser = Prs g where
        g s = case runPrs parser s of
                  Nothing -> Nothing
                  Just (c, s) -> Just (f c, s)

instance Applicative Prs where
  pure v = Prs $ \s -> Just (v, s)
  Prs pf <*> Prs pv = Prs g where
    g s = do
        (f, s1) <- pf s
        (v, s2) <- pv s1
        return (f v, s2)
