import Control.Applicative
import Data.Char (isDigit)
newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat

char :: Char -> Prs Char
char c = Prs f where
    f "" = Nothing
    f (x:xs) | x == c = Just (c, xs)
             | otherwise = Nothing


nat :: Prs Int
nat = Prs f where
    f s = do
       (digits, rest) <- runPrs (many1 digit) s
       return (read digits :: Int, rest)

digit :: Prs Char
digit = Prs f where
    f ""     = Nothing
    f (x:xs) = if isDigit x then Just (x, xs) else Nothing


tst = [
    runPrs mult "14*3" == Just (42,""),
    runPrs mult "64*32" == Just (2048,""),
    runPrs mult "77*0" == Just (0,""),
    runPrs mult "2*77AAA" == Just (154,"AAA")
    ]

many1 :: Prs a -> Prs [a]
many1 p = pure (:) <*> p <*> (many1 p <|> pure [])


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

instance Alternative Prs where
  empty = Prs $ \_ -> Nothing
  (Prs l) <|> (Prs r) = Prs f where
    f s = case l s of
      Nothing -> r s
      x -> x
