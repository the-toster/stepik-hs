{-
0: 0     = 0
1: 1 + 0 = 1
2: 1 + 1 = 2
3: 2 + 1 = 3
4: 3 + 2 = 5
5: 5 + 3 = 8
6: 8 + 5 = 13
-}


fib :: Int -> Int
fib n | n == 0      = 0
      | n > 0       = calcFib 0 1 n
      | n < 0       = calcFibNeg 0 1 n


calcFib preLast last 1 = 1
calcFib preLast last 2 = preLast + last
calcFib preLast last n = calcFib last (preLast + last) (n - 1)


calcFibNeg preLast last (-1) = 1
calcFibNeg preLast last (-2) = preLast - last
calcFibNeg preLast last n = calcFibNeg last (preLast - last) (n + 1)
