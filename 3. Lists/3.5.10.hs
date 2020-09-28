evenOnly :: [a] -> [a]
evenOnly = (map snd) . filter (\(n, x) -> even n) . zip [1..]
evenOnly = foldr (hlp) [([], True)]

hlp x acc = ([x], length acc)

tst1 = evenOnly [1..10]
tst2 = evenOnly [1..9]
tst3 = take 4 $ evenOnly [1..]
tst = [tst1, tst2, tst3]
