import Data.List.Split (splitOn)
import Data.Char (isDigit)
data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show
type RecordSet = [(String, String)]
type ParsingResult = Either Error RecordSet

parsePerson :: String -> Either Error Person
parsePerson = buildPerson . validateRecords . getRecords . items

buildPerson :: ParsingResult -> Either Error Person
buildPerson (Left e) = Left e
buildPerson (Right records) = if
                         has records "firstName" &&
                         has records "lastName" &&
                         has records "age"
                         then Right $ Person (get records "firstName") (get records "lastName") (read $ get records "age")
                         else Left IncompleteDataError

validateRecords :: ParsingResult -> ParsingResult
validateRecords (Left e) = Left e
validateRecords (Right records) = if has records "age" then
                                    if all (isDigit) (get records "age")
                                    then Right records else Left $ IncorrectDataError (get records "age")
                                    else Left IncompleteDataError

getRecords :: [[String]] -> ParsingResult
getRecords [] = Right []
getRecords ([a, b]:xs) = case getRecords xs of
                            Left e -> Left e
                            Right records -> Right $ (a, b) : records
getRecords _ = Left ParsingError


items :: String -> [[String]]
items inp = map (splitOn " = ") $ lines inp


has :: RecordSet -> String -> Bool
has records key = any (\(k, v) -> k == key) records

get :: RecordSet -> String -> String
get (x:xs) k = if fst x == k then snd x else get xs k


input = "firstName = John\nlastName = Connor\nage = 30a"
tst = parsePerson input
