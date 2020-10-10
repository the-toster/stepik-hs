qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort l ++ x : qsort g where
                (l, g) = partition (x>) xs
                partition p xs = (filter p xs, filter (not . p) xs)
