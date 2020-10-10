data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering

cmp a b = compare (toInt a) (toInt b) where
    toInt Error   = 3
    toInt Warning = 2
    toInt Info    = 1

tst = [
        (cmp Error Warning)   == GT,
        (cmp Info Warning)    == LT,
        (cmp Warning Warning) == EQ
    ]
