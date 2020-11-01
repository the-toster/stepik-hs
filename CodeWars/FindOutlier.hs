module FindOutlier (findOutlier) where

findOutlier :: [Int] -> Int
findOutlier n@(x:y:z:xs) = foldl (solver f) x n where
    solver f a b = if f a then b else a
    f = if (odd x && odd y) ||
           (odd y && odd z) ||
           (odd x && odd z)
           then odd else even
