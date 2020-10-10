groupElems :: Eq a => [a] -> [[a]]
groupElems xs = reverse (iter xs []) where
    iter [] acc = acc
    iter (x:xs) acc = iter xs (grouper x acc)
    grouper x [] = [[x]]
    grouper x ((g:gs):other) = if x == g then (x:g:gs) : other else [x] : (g:gs) : other
