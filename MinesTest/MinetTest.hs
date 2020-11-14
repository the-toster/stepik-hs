module MinerTest where
import Data.List (transpose)

import TaskTypes -- Ходы и координаты
import MinesOrDieAndrey as A -- Андрея
import EscapeTheMinesOrDie as B (solve) -- Мое




-- это запустить в ghci
drw = visualSolve B.solve charMap (0, 0) (10, 10)

-- рисовалка, там можно вызвать transpose, перед prettyPrint, чтобы повернуть карту, но все равно непонятно выглядит
visualSolve solver charMap start door =
    let
        m = unmap charMap
        solution = solver m start door
    in case solution of
        Nothing -> putStr "No solution found"
        Just moves -> putStr $ prettyPrint $ drawSolution charMap start door moves

-- тесты последнего лабиринта
tst = [
        B.solve m  (0, 0) (11, 11) == Just [D,D,R,R,U,U,R,R,D,D,R,R,U,U,R,R,D,D,R,R,R,D,D,L,D,L,L,L,U,L,L,D,L,L,U,L,L,D,L,D,D,R,D,D,L,D,D,R,R,R,U,U,U,U,R,R,D,D,R,R,D,R,R,R,R,D],
        A.solve m  (0, 0) (11, 11) == Just [D,D,R,R,U,U,R,R,D,D,R,R,U,U,R,R,D,D,R,R,R,D,D,L,D,L,L,L,U,L,L,D,L,L,U,L,L,D,L,D,D,R,D,D,L,D,D,R,R,R,U,U,U,U,R,R,D,D,R,R,D,R,R,R,R,D],
        B.solve m  (0, 0) (10, 10) == Just [D,D,R,R,U,U,R,R,D,D,R,R,U,U,R,R,D,D,R,R,R,D,D,L,D,L,L,L,U,L,L,D,L,L,U,L,L,D,L,D,D,R,D,D,L,D,D,R,R,R,U,U,U,U,R,R,D,D,R,R,D,R,R,R],
        A.solve m  (0, 0) (10, 10) == Just [D,D,R,R,U,U,R,R,D,D,R,R,U,U,R,R,D,D,R,R,R,D,D,L,D,L,L,L,U,L,L,D,L,L,U,L,L,D,L,D,D,R,D,D,L,D,D,R,R,R,U,U,U,U,R,R,D,D,R,R,D,R,R,R]
      ]


charMap =               ["   ##   #   "
                          ,"## #  #   # "
                          ,"   # ###### "
                          ," ###  #     "
                          ,"   ## # ####"
                          ,"## #  #     "
                          ,"   # #### ##"
                          ," ###  #     "
                          ,"   ## #### #"
                          ," # #@ #  #  "
                          ," # #  #    #"
                          ,"#    #  #   "]

m = unmap charMap
unmap = map (map (== ' '))

prettyPrint :: [String] -> String
prettyPrint [] = ""
prettyPrint (x:xs) = x ++ '\n' : prettyPrint xs

drawSolution :: [[Char]] -> XY -> XY -> [Move] -> [[Char]]
drawSolution m start door [] = modifyMap m door 'E'
drawSolution m start door (x:xs) =
    let
        nextMap = modifyMap m start (head $ show x)
        nextPos = step start x
        in drawSolution nextMap nextPos door xs

modifyMap :: [[a]] -> XY -> a -> [[a]]
modifyMap mines (x, y) a = setByIndex mines x (setByIndex (mines !! x) y a)

setByIndex :: [a] -> Int -> a -> [a]
setByIndex xs ind v = begin ++ (v : tail end) where
    (begin, end) = splitAt ind xs


step :: XY -> Move -> XY
step (x, y) U = (x, y - 1)
step (x, y) D = (x, y + 1)
step (x, y) R = (x + 1, y)
step (x, y) L = (x - 1, y)
