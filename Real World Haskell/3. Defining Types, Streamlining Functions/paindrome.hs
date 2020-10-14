isPalindrome xs = xs == reverse xs
-- I think this version is more interesting then just xs ++ reverse xs
buildPalindrome xs | isPalindrome xs = xs
                   | otherwise = xs ++ (tail $ reverse xs)
