nTimes:: a -> Int -> [a]
nTimes el times | times < 0     = error "only non-negative length possible"
                | times == 0    = []
                | otherwise     =  el : nTimes el (times - 1)

-- позже прочитал, что по соглашению стандартной библиотеки, на отрицательных аргументах должен быть пустой список
