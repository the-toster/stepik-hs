sumOdd :: [Integer] -> Integer
sumOdd a = foldr (+) 0 $ filter odd a
