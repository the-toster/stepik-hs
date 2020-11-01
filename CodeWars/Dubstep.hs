module Dubstep where
import Data.List.Split (splitOn)

songDecoder :: String -> String
songDecoder str = unwords $ filter ( /= "") (splitOn "WUB" str)


tst = [
          songDecoder "AWUBBWUBC" == "A B C",
          songDecoder "AWUBWUBWUBBWUBWUBWUBC" == "A B C",
          songDecoder "WUBAWUBBWUBCWUB" == "A B C",
          songDecoder "WUBWEWUBAREWUBWUBTHEWUBCHAMPIONSWUBMYWUBFRIENDWUB"
            == "WE ARE THE CHAMPIONS MY FRIEND"
            ]
