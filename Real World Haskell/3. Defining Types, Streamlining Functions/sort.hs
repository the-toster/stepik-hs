import Data.List (sortBy)

srt :: [[a]] -> [[a]]
srt xs = sortBy (\a b -> compare (length a) (length b)) xs
