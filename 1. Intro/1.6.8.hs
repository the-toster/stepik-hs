sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (s, c) where
        (s, c) = calcSum 0 0 (abs x)
        calcSum acc cnt x =
            if x >= 10 then
                calcSum (resedu x + acc) (cnt + 1) (x `div` 10)
            else (x + acc, cnt + 1)
        resedu x = x `mod` 10
