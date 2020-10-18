evalWriter :: Writer w a -> a
evalWriter w = fst $ runWriter w
