integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * ( (f a + f b) / 2 + sumOfCalls 1 ) where
    h = (b - a) / n
    n = 1000
    sumOfCalls number = if number == n then 0 else f (a + number * h) + sumOfCalls (number + 1)

