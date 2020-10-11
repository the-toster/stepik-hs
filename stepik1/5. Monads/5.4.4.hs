import Data.Char (isDigit)
data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
     deriving (Eq, Show)
-- Тип Token уже объявлен, его писать не нужно

asToken :: String -> Maybe Token
asToken c | all isDigit c = Just $ Number $ read c
          | c == "+"      = Just Plus
          | c == "-"      = Just Minus
          | c == "("      = Just LeftBrace
          | c == ")"      = Just RightBrace
          | otherwise     = Nothing

tokenize :: String -> Maybe [Token]
tokenize input = mapM (asToken) (words input)


tst = [
        asToken "123" == Just (Number 123),
        asToken "abc" == Nothing
    ]

tst1 = [
        tokenize "1 + 2" == Just [Number 1,Plus,Number 2],
        tokenize "1 + ( 7 - 2 )" == Just [Number 1,Plus,LeftBrace,Number 7,Minus,Number 2,RightBrace],
        tokenize "1 + abc" == Nothing
    ]
