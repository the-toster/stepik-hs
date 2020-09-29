data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance a b = sqrt $ sum $ map (^2) $ diffs a b

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance a b = sum $ map (abs) $ diffs a b

diffs (Coord x y) (Coord x1 y1) = [x - x1, y - y1]
