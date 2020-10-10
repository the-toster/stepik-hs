seqA :: Integer -> Integer
seqA n = let
    coreCalc a2 a1 a0 = a2 + a1 - 2 * a0
    calc a2 a1 a0 0 = 1
    calc a2 a1 a0 1 = 2
    calc a2 a1 a0 2 = 3
    calc a2 a1 a0 3 = coreCalc a2 a1 a0
    calc a2 a1 a0 n = calc (coreCalc a2 a1 a0) a2 a1 (n - 1)
    in calc 3 2 1 n



