
highAndLow :: String -> String
highAndLow input =  highest ++ " " ++ lowest where
                    lowest  = show $ foldl (min) first ints
                    highest = show $ foldl (max) first ints
                    ints    = getInts input
                    first   = head ints

getInts :: String -> [Int]
getInts input = map read $ words input
