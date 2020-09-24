evenOnly :: [a] -> [a]
evenOnly xs = fst $ foldr (hlp) ([], odd $ length xs) xs
hlp x acc = (
        if snd acc then x : (fst acc) else fst acc,
        not (snd acc)
    )
tst1 = evenOnly [1..10]
tst2 = evenOnly [1..9]
tst3 = take 4 $ (evenOnly [1..])
----
tst = [tst1, tst2, tst3]
