max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\a b c-> max a $ max b c)
