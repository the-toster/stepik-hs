
data Point = Point Int Int
data Vec  =  Vec   Int Int


pointTurn :: Point -> Point -> Point -> Int
pointTurn p1 p2 p3 = vecTurn (vec p1 p2) (vec p2 p3)

vec :: Point -> Point -> Vec
vec (Point x1 y1) (Point x2 y2) = Vec (x2 - x1) (y2 -y1)

vecTurn :: Vec -> Vec -> Int
vecTurn a b = (vecMult a b) `div` (vecMod a * vecMod b)

vecMult (Vec x1 y1) (Vec x2 y2) = (x1 * x2) + (y1 * y2)
vecMod  (Vec x y)               = sqrt (x^2 + y^2)


tst = vecTurn (Vec 5 5) (Vec 5 5)
