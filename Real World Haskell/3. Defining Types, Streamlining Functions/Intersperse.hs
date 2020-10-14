intersperse :: a -> [[a]] -> [a]

intersperse _ [] = []
intersperse _ [a] = a
intersperse s (x:xs) = x ++ (s : (intersperse s xs))

