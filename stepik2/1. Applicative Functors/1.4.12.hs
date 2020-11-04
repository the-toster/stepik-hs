instance Alternative Prs where
  empty = Prs $ \_ -> Nothing
  (Prs l) <|> (Prs r) = Prs f where
    f s = case l s of
      Nothing -> r s
      x -> x
