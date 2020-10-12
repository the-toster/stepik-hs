data Board = Board Int deriving Show

nextPositions :: Board -> [Board]
nextPositions (Board i) = map Board [succ i, succ $ succ i]

tst = nextPositionsN (Board 1) 2 (\ (Board n) -> even n || True)

nextPositionsN b 0 pred = filter pred [b]
nextPositionsN b 1 pred = filter pred (nextPositions b)
nextPositionsN b n pred = do
    if n < 1 then fail "recursion stop"
        else do
            position <- nextPositions b
            nextPositionsN position (n -1) pred

--
nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
--
--nextPositionsN b n pred | n < 1 = []
--                        | otherwise =
