module EscapeTheMinesOrDie where
import Data.List (sortBy)
import Data.Maybe (catMaybes)

import TaskTypes

solve :: [[Bool]] -> XY -> XY -> Maybe [Move]
solve mines pos door = if badPosition mines pos then Nothing
                       else if doorPosition pos door then Just []
                       else let
                            newMines = visit mines pos
                            newSolve = \pos -> solve newMines pos door
                            in getShortest $ map (\m -> fmap (m:) (newSolve (step pos m)) ) [U, D, R, L]


badPosition mines (x, y) = outOfBounds mines x y || isBomb mines x y where
    isBomb mines x y = not $ (mines !! x) !! y
    outOfBounds :: [[Bool]] -> Int -> Int -> Bool
    outOfBounds mines x y = x < 0 || y < 0 || x >= length mines || y >= length (head mines)

doorPosition = (==)

visit :: [[Bool]] -> XY -> [[Bool]]
visit mines (x, y) = setByIndex mines x (setByIndex (mines !! x) y False)

setByIndex :: [a] -> Int -> a -> [a]
setByIndex xs ind v = begin ++ (v : tail end) where
    (begin, end) = splitAt ind xs

step :: XY -> Move -> XY
step (x, y) U = (x, y - 1)
step (x, y) D = (x, y + 1)
step (x, y) R = (x + 1, y)
step (x, y) L = (x - 1, y)

getShortest :: [Maybe [Move]] -> Maybe [Move]
getShortest v = if null vars then Nothing else Just $ head vars where
    vars = sortBy (\a b -> compare (length a) (length b)) (catMaybes v)



m = unmap ["   ##"
          ,"## # "
          ,"     "
          ," # ##"
          ,"#    "]

tst = solve m (0, 0) (4, 4) == (Just [D, D, R, R, R, R, D, D])
unmap = map (map (== ' '))
t = badPosition m (5, 0)
