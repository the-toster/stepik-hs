{-
f1 :: a -> (a,b) -> a -> (b,a,a)
f1 :: Int -> (Int,Bool) -> Int -> (Bool,Int,Int)

f1 a1 (a2,b) a3 = (b, a1, a1)
f2 a1 (a2,b) a3 = (b, a1, a2)
f3 a1 (a2,b) a3 = (b, a1, a3)

f4 a1 (a2,b) a3 = (b, a2, a1)
f5 a1 (a2,b) a3 = (b, a2, a2)
f6 a1 (a2,b) a3 = (b, a2, a3)

f7 a1 (a2,b) a3 = (b, a3, a1)
f8 a1 (a2,b) a3 = (b, a3, a2)
f9 a1 (a2,b) a3 = (b, a3, a3)
-}
