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
{-
после некоторого тупежа, вспомнил подобные задачи из практики, и подход, видимо наивный
который заключается в развитии мысли о том, что любая перестановка начинается с какого-то элемента,
т.е. все возможные перестановки начинаются с одного из данных эл. и состоят из оставшихся.
Но его реализовать с наскока не получилось.

Следующей мыслью было, что перестановка n+1 элементов это
все перестановки из n, только с еще одним элементом на всех местах.

-}