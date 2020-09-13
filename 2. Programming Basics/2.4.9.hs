avg :: Int -> Int -> Int -> Double
avg a b c = (fromIntegral a + fromIntegral b + fromIntegral c) / 3
-- fromIntegral (a + b + c) - видимо переполняется
