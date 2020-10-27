import Control.Applicative

divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = (show 1.0, 1)
divideList' (x:xs) = (/) <$> ("<-" ++ show x ++ "/", x) <*> divideList' xs

tst = divideList' [3,4,5] == ("<-3.0/<-4.0/<-5.0/1.0",3.75)
