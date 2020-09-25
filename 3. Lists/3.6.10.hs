revRange :: (Char,Char) -> [Char]
revRange = unfoldr g
  where g (f, t) = if f > t then Nothing else Just(t, (f, pred t))
