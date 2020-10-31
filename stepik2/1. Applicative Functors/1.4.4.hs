newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }


anyChr :: Prs Char
anyChr = Prs f where
    f "" = Nothing
    f (x:xs) = Just (x, xs)

instance Functor Prs where
    fmap f parser = Prs g where
        g s = case runPrs parser s of
                  Nothing -> Nothing
                  Just (c, s) -> Just (f c, s)
