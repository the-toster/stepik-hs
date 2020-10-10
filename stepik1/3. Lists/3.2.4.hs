filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj f1 f2 l = filter (\x -> f1 x || f2 x) l
