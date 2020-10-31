module Codewars.Kata.Dubstep where

songDecoder :: String -> String
songDecoder = error "todo: songDecoder"


tst = [
          songDecoder "AWUBBWUBC" == "A B C",
          songDecoder "AWUBWUBWUBBWUBWUBWUBC" == "A B C",
          songDecoder "WUBAWUBBWUBCWUB" == "A B C",
          songDecoder "WUBWEWUBAREWUBWUBTHEWUBCHAMPIONSWUBMYWUBFRIENDWUB"
            == "WE ARE THE CHAMPIONS MY FRIEND"
            ]
