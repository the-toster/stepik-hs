sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 (a:as) (b:bs) (c:cs) = (a + b + c) : sum3 as bs cs
sum3 (a:as) (b:bs) []     = (a + b) : sum3 as bs []
sum3 (a:as) [] (c:cs)     = (a + c) : sum3 as [] cs
sum3 [] (b:bs) (c:cs)     = (b + c) : sum3 [] bs cs

sum3 [] [] (c:cs)         = c : sum3 [] [] cs
sum3 (a:as) [] []         = a : sum3 as [] []
sum3 [] (b:bs) []         = b : sum3 [] bs []

sum3 [] [] []             = []


-- криво конечно, потом увидел другие решения, надо было передавать [0] вместо пустых, было бы на три шаблона меньше
