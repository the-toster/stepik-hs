module TaskTypes where

type XY = (Int,Int)
data Move = U | D | R | L deriving (Eq, Show, Read)
