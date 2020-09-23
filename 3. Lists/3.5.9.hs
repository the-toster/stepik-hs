evenOnly :: [a] -> [a]
evenOnly xs = reverse $ fst $ hlp xs where
        hlp = foldl (\(items, ind) x -> if odd ind then (x:items, ind + 1) else (items, ind + 1)) ([], 0)

