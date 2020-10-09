module Narcissistic where

narcissistic :: Integral n => n -> Bool
narcissistic n = n == calc n numberOfDigits where
  numberOfDigits = 1 + (floor $ logBase 10 (fromIntegral n))
  calc 0 _ = 0
  calc n p = calc (div n 10) p + (mod n 10) ^ p


{-
не все решения отсюда я понимаю..
https://www.codewars.com/kata/5287e858c6b5a9678200083c/solutions/haskell
-}
