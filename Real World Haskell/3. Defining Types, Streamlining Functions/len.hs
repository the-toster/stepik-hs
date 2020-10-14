len :: [a] -> Integer

len [] = 0
len (x:xs) = calc 1 xs where
    calc s xs = s + len xs
