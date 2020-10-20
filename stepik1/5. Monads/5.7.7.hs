import Data.Monoid

newtype Writer w a = Writer { runWriter :: (a, w) } deriving Show

writer :: (a, w) -> Writer w a
writer = Writer

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    m >>= k  =
        let (x, u) = runWriter m
            (y, v) = runWriter $ k x
        in Writer (y, u `mappend` v)

data Log = Log {getItems :: [String], getTotal :: Sum Integer}

instance Monoid Log where
 mempty = Log [] (Sum 0)
 mappend (Log l1 s1) (Log l2 s2) = Log (mappend l1 l2) (mappend s1 s2)

type Shopping = Writer Log ()

purchase :: String -> Integer -> Shopping
purchase item cost = writer ((), Sum cost)

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328


total :: Shopping -> Integer
total = getSum . getTotal . execWriter

items :: Shopping -> [String]
items = getItems . execWriter
