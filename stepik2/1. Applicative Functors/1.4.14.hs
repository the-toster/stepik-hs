many1 :: Prs a -> Prs [a]
many1 p = pure (:) <*> p <*> (many1 p <|> pure [])
