module DirectionCalc where

data Direction = LeftTurn | RightTurn | Straight deriving (Show, Eq)
data Point = Point Double Double
{-
 начал делать через угол между векторами, но появляются проблемы с точностью в случае если он == 0
 прочитал в одном месте - https://www.geeksforgeeks.org/direction-point-line-segment/
 про cross product of two points, что на мой взгляд переводится как векторное произведение двух точек
 никогда о таком не слышал.
 Но попробую как там написано.
-}

calcDirection :: Point -> Point -> Point -> Direction
calcDirection a b c = directionOfProduct $ crossProduct (toBasis a b) (toBasis a c)

directionOfProduct p | p < 0  = RightTurn
                     | p > 0  = LeftTurn
                     | p == 0 = Straight

crossProduct (Point x1 y1) (Point x2 y2) = (x1 * y2) - (x2 * y1)
toBasis (Point toX toY) (Point x y) = Point (x - toX) (y - toY)


pA = Point 10 10
pB = Point 20 20
pC = Point 30 30
pL = Point 20 40
pR = Point 20 10
tst = [
        calcDirection pA pB pC == Straight,
        calcDirection pA pB pL == LeftTurn,
        calcDirection pA pB pR == RightTurn
    ]

{-
-- вариант с углом

data Vec  =  Vec Double Double

pointTurn p1 p2 p3 = vecTurn (vec p1 p2) (vec p2 p3)

vec :: Point -> Point -> Vec
vec (Point x1 y1) (Point x2 y2) = Vec (x2 - x1) (y2 -y1)

vecTurn :: Vec -> Vec -> Int
vecTurn a b = (vecMult a b) `div` (vecMod a * vecMod b)

vecMult (Vec x1 y1) (Vec x2 y2) = (x1 * x2) + (y1 * y2)
vecMod  (Vec x y)               = sqrt (x^2 + y^2)

tst = vecTurn (Vec 5 5) (Vec 5 5)
-}


