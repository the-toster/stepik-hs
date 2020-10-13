pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do
    c <- [1 .. x]
    b <- [1 .. c]
    a <- [1 .. b]
    True <- return $ (a^2 + b^2) == c^2
    return (a, b, c)

tst = [
        pythagoreanTriple 5 == [(3,4,5)],
        pythagoreanTriple 0 == [],
        pythagoreanTriple 10 == [(3,4,5),(6,8,10)]
    ]
