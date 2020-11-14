module MinerTest where
import Data.List (transpose)

import TaskTypes -- Ходы и координаты
import MinesOrDieAndrey as A -- Андрея
import EscapeTheMinesOrDie as B (solve) -- Мое


start = (0, 0)
door1 = (11, 11)
door2 = (3, 0)

-- это запустить в ghci
drw = visualSolve A.solve charMap2 start door2
--drw = visualSolve B.solve charMap (0, 0) (3, 10)

-- рисовалка, там можно вызвать transpose, перед prettyPrint, чтобы повернуть карту, но все равно непонятно выглядит
visualSolve solver charMap start door =
    let
        m = unmap charMap
        solution = solver m start door
        visual = \m -> prettyPrint $ drawSolution charMap start door m
    in case solution of
        Nothing -> putStrLn $ "No solution found\n" ++ (visual [])
        Just moves -> putStrLn $ visual moves

-- тесты последнего лабиринта
tst = [
        (B.solve m2 start door2) == (A.solve m2 start door2)
--        B.solve m  (0, 0) (11, 11) == Just [D,D,R,R,U,U,R,R,D,D,R,R,U,U,R,R,D,D,R,R,R,D,D,L,D,L,L,L,U,L,L,D,L,L,U,L,L,D,L,D,D,R,D,D,L,D,D,R,R,R,U,U,U,U,R,R,D,D,R,R,D,R,R,R,R,D],
--        A.solve m  (0, 0) (11, 11) == Just [D,D,R,R,U,U,R,R,D,D,R,R,U,U,R,R,D,D,R,R,R,D,D,L,D,L,L,L,U,L,L,D,L,L,U,L,L,D,L,D,D,R,D,D,L,D,D,R,R,R,U,U,U,U,R,R,D,D,R,R,D,R,R,R,R,D],
--        B.solve m  (0, 0) (10, 10) == Just [D,D,R,R,U,U,R,R,D,D,R,R,U,U,R,R,D,D,R,R,R,D,D,L,D,L,L,L,U,L,L,D,L,L,U,L,L,D,L,D,D,R,D,D,L,D,D,R,R,R,U,U,U,U,R,R,D,D,R,R,D,R,R,R],
--        A.solve m  (0, 0) (10, 10) == Just [D,D,R,R,U,U,R,R,D,D,R,R,U,U,R,R,D,D,R,R,R,D,D,L,D,L,L,L,U,L,L,D,L,L,U,L,L,D,L,D,D,R,D,D,L,D,D,R,R,R,U,U,U,U,R,R,D,D,R,R,D,R,R,R]
      ]

charMap2 =               [
                           "  "
                          ," #"
                          ,"  "
                          ," #"
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
m2 = unmap charMap2
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
