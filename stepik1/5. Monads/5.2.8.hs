data Log a = Log [String] a

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log l1 a) f = Log (l1 ++ l2) b where
    Log l2 b = f a

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList a l = foldl (>>=) (return a) l

