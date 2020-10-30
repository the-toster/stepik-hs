import Text.Parsec

getList :: Parsec String u [String]
getList = sepBy1 (many1 digit) (char ';')
