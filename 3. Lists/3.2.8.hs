{-
Воспользовавшись функциями map и concatMap, определите функцию perms,
которая возвращает все перестановки, которые можно получить из данного списка, в любом порядке.

GHCi> perms [1,2,3]
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

Считайте, что все элементы в списке уникальны, и что для пустого списка имеется одна перестановка.
-}

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concatMap (\variants -> inserter x variants (length variants)) (perms xs) where
    inserter el xs p | p == (-1) = []
                     | otherwise = (insertAt p el xs) : inserter el xs (p - 1)
    insertAt p el xs = b ++ el : e where (b, e) = splitAt p xs
