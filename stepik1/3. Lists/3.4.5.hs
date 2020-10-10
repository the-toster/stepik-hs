lengthList :: [a] -> Int
lengthList = foldr (\_ y -> 1 + y) 0
