module MinesMonadic where

import Data.Maybe
import Data.List

type XY = (Int, Int)
data Move = U | D | R | L deriving (Eq, Show)

solve :: [[Bool]] -> XY -> XY -> Maybe [Move]
solve mines pos door = do
    assertPosition mines pos
    if pos == door then return [] else shortestOf variants
        where variants = map (\m -> fmap (m:) $ solve (visit mines pos) (step pos m) door) [U, D, R, L]



assertPosition mines (x, y) = if outOfBounds || isBomb then Nothing else Just () where
    outOfBounds = x < 0 || y < 0 || x >= length mines || y >= length (head mines)
    isBomb = not $ (mines !! x) !! y

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

shortestOf variants = if null vars then Nothing else Just $ head vars where
                          vars = sortBy (\a b -> compare (length a) (length b)) (catMaybes variants)



unmap = map (map (== ' '))
m = unmap ["   ##"
          ,"## # "
          ,"     "
          ," # ##"
          ,"#    "]

tst = solve m (0, 0) (4, 4) == (Just [D, D, R, R, R, R, D, D])
