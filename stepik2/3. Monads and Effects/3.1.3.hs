withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept t me = case runExcept me of
    Left e -> except $ Left $ t e
    Right a -> except $ Right a
