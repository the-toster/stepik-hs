data Log a = Log [String] a

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log l1 a) f = Log (l1 ++ l2) b where
    Log l2 b = f a
