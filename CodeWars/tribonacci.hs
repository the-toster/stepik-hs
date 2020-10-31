tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) n = reverse $ tbn (a, b, c) n
tbn (a, b, c) 0 = [];
tbn (a, b, c) 1 = [a];
tbn (a, b, c) 2 = [b, a];
tbn (a, b, c) 3 = [c, b, a];
tbn (a, b, c) n = (sum $ take 3 rest) : rest where rest = tbn (a, b, c) (n - 1);

tst = [
        tribonacci (1, 1, 1) 10 == [1,1,1,3,5,9,17,31,57,105],
        tribonacci (0, 0, 1) 10 == [0,0,1,1,2,4,7,13,24,44],
        tribonacci (0, 1, 1) 10 == [0,1,1,2,4,7,13,24,44,81]
    ]
