module TitleCase (titleCase) where
import Data.Char (toLower, toUpper)

titleCase :: String -> String -> String
titleCase _     ""    = ""
titleCase minor title = unwords $ titleWord firstWord : map conditionalTitleWord titleTail where
    firstWord = head $ words $ toLowerCase title
    titleTail = tail $ words $ toLowerCase title
    titleWord (x:xs) = toUpper x : toLowerCase xs
    conditionalTitleWord wrd = if wrd `elem` exceptions then wrd else titleWord wrd
    toLowerCase w = map (toLower) w
    exceptions = words $ toLowerCase minor


tst = titleCase "a an the of" "a clash of KINGS" == "A Clash of Kings"
