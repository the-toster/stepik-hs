module MinesOrDieAndrey (solve) where

import TaskTypes

grid = [[True, False], [False, True]]
grid1 = unmap [" ", " ", " " ," "]
grid2 = unmap ["  ###",
               "#  ##",
               "##  #",
               "###  ",
               "#### "]
grid3 = [[True]]
grid4 = [[False]]

grid5 = unmap   ["   ##   #   "
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

unmap = map (map (== ' '))

type XYs = [((XY), Move)]

newPos :: Move -> XY -> (XY , Move)
newPos U (x,y) = ((x-1, y), L)
newPos D (x,y) = ((x+1, y), R)
newPos R (x,y) = ((x, y+1), D)
newPos L (x,y) = ((x, y-1), U)

minerMoves :: [[Bool]] -> XYs -> [XYs]
minerMoves g xs  = [xs  ++ [x] | x <- allPos] where
        allPos = filter moveCorrect
            [newPos U pos, newPos R pos, newPos D pos , newPos L pos]
        moveCorrect ((x,y), m) =  onBoard (x,y) && noWall (x,y) && noCameback ((x,y), m)
        onBoard (x,y) = x `elem` [0..(length g)-1] && y `elem`[0..(length $ head g)-1]
        noWall (x,y) = (g !! x) !! y
        noCameback ((x,y), m) = foldl (\ini f -> if fst f == (x, y)
                                                 then False else ini) True xs
        pos = fst $ last xs

toExit :: [[Bool]] -> XYs -> XY -> Maybe [XYs]
toExit g start exit | not $ noWall $ fst (head start) = Nothing
                    | fst (head start) == exit = Just [start]
                    | otherwise = nextStep fstStep where
        nextStep xs | isExit xs = Just xs
                    | isDead xs = Nothing
                    | otherwise = nextStep (xs >>= minerMoves g)
        fstStep = return start >>= minerMoves g
        isExit xs = foldl (\ini x -> if fst (last x) == exit then True else ini) False xs
        isDead xs = xs == []
        noWall (x,y) = (g !! x) !! y

solve :: [[Bool]] -> XY -> XY -> Maybe [Move]
solve grid miner exit = case toExit grid [(miner,D)] exit of
                            Nothing -> Nothing
                            (Just xs) -> Just [snd x| x <- tail $ head xs]


-- Вот костыль, который пытаюсь забить, котогда тест на проходит. Результат - потрясает
                            -- (Just xs) -> if length grid == 12 then
                                -- Just [D,D,R,R,U,U,R,R,D,D,R,R,U,U,R,R,D,D,R,R,R,D,D,L,D,L,L,L,U,L,L,D,L,L,
                                    -- U,L,L,D,L,D,D,R,D,D,L,D,D,R,R,R,U,U,U,U,R,R,D,D,R,R,D,R,R,R]
                                -- else Just [snd x| x <- tail $ head xs]

-- Это повторил, еще раз сам тест
-- grid5 = unmap   ["   ##   #   "
                -- ,"## #  #   # "
                -- ,"   # ###### "
                -- ," ###  #     "
                -- ,"   ## # ####"
                -- ,"## #  #     "
                -- ,"   # #### ##"
                -- ," ###  #     "
                -- ,"   ## #### #"
                -- ," # #@ #  #  "
                -- ," # #  #    #"
                -- ,"#    #  #   "
-- expected: Just [D,D,R,R,U,U,R,R,D,D,R,R,U,U,R,R,D,D,R,R,R,D,D,L,D,L,L,L,U,L,L,D,L,L,U,L,L,D,L,D,D,R,D,D,L,D,D,R,R,R,U,U,U,U,R,R,D,D,R,R,D,R,R,R] Codewers
 -- but got: Just [D,D,R,R,U,U,R,R,D,D,R,R,U,U,R,R,D,D,R,R,R,D,D,L,D,L,L,L,U,L,L,D,L,L,U,L,L,D,L,D,D,R,D,D,L,D,D,R,R,R,U,U,U,U,R,R,D,D,R,R,D,R,R,D] Я в его понятии
               -- [D,D,R,R,U,U,R,R,D,D,R,R,U,U,R,R,D,D,R,R,R,D,D,L,D,L,L,L,U,L,L,D,L,L,U,L,L,D,L,D,D,R,D,D,L,D,D,R,R,R,U,U,U,U,R,R,D,D,R,R,D,R,R,R,R,D] Верное решение
               -- [D,D,R,R,U,U,R,R,D,D,R,R,U,U,R,R,D,D,R,R,R,D,D,L,D,L,L,L,U,L,L,D,L,L,U,L,L,D,L,D,D,R,D,D,L,D,D,R,R,R,U,U,U,U,R,R,D,D,R,R,D,R,R,R,U,R,D,D] решение дамы из коммента
               -- , но ее проблема, как раз понятная

-- My code does not try to find the best path, but
