module Fib where
import Data.List.Split (chunksOf)

productFib :: Integer -> (Integer, Integer, Bool)
productFib n = let
                (fn1, fn2) = head $ dropWhile (\(fn1, fn2) -> fn1 * fn2 < n) pairs
                in (fn1, fn2, fn1 * fn2 == n)

pairs = zip fibs (tail fibs)
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
