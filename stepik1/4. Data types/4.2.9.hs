import Data.List

data Bit = Zero | One deriving (Show, Read, Eq)
data Sign = Minus | Plus deriving (Show, Read, Eq)
data Z = Z Sign [Bit] deriving (Show, Read, Eq)

add :: Z -> Z -> Z
add a b = toZ $ toInt a + toInt b

mul :: Z -> Z -> Z
mul a b = toZ $ toInt a * toInt b

toInt :: Z -> Integer
toInt (Z Plus bits) = foldr hlp 0 $ zip bits [0..] where
                hlp (Zero, _) s = s
                hlp (One, pow) s = (2^pow) + s
toInt (Z Minus bits) = (-1) * toInt (Z Plus bits)

toZ :: Integer -> Z
toZ a | a >= 0    = Z Plus  $ bits a
      | otherwise = Z Minus $ bits $ abs a where
            bits :: Integer -> [Bit]
            bits a = unfoldr hlp a where
                        hlp 0 = Nothing
                        hlp a = Just (toBit $ a `mod` 2, a `div` 2)
                        toBit 1 = One
                        toBit 0 = Zero
