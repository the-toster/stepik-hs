data Coord a = Coord a a deriving Show

getCenter :: Double -> Coord Int -> Coord Double
getCenter w (Coord a b) = Coord (cnt a) (cnt b) where cnt x = w * (0.5 + fromIntegral x)

getCell :: Double -> Coord Double -> Coord Int
getCell w (Coord a b) = Coord (hlp a) (hlp b) where hlp x = floor (x / w)


tst = Coord 1.2 1.2
