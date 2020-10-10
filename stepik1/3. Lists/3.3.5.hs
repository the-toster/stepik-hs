{-
repeat = iterate repeatHelper
-}
rpt = iterate repeatHelper
repeatHelper = id
test = "qqqq" == (take 4 $ rpt 'q')
