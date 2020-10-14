-- mean :: Num a => [a] -> Double

mean xs = (sum xs) / (fromIntegral $ length xs)
