{-
Реализуйте функцию parsePerson, которая разбирает строки вида firstName = John\nlastName = Connor\nage = 30
и возвращает либо результат типа Person, либо ошибку типа Error.

    Строка, которая подается на вход, должна разбивать по символу '\n' на список строк,
        каждая из которых имеет вид X = Y. Если входная строка не имеет указанный вид, то функция должна возвращать ParsingError.
    Если указаны не все поля, то возвращается IncompleteDataError.
    Если в поле age указано не число, то возвращается IncorrectDataError str, где str — содержимое поля age.
    Если в строке присутствуют лишние поля, то они игнорируются.

-}

{-
    на этой задаче сначала ощутил ступор от нехватки мутабельного стейта, но когда родил
        parsePerson = buildPerson . validateRecords . getRecords . parseStrings
    все стало понятно, и почувствовалась некоторая легкость и конкретность/выразительность
-}

import Data.List.Split (splitOn)
import Data.Char (isDigit)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show
data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

type RecordSet = [(String, String)]
type ParsingResult = Either Error RecordSet

parsePerson :: String -> Either Error Person
parsePerson = buildPerson . validateRecords . getRecords . parseStrings

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


parseStrings :: String -> [[String]]
parseStrings inp = map (splitOn " = ") $ lines inp


has :: RecordSet -> String -> Bool
has records key = any (\(k, v) -> k == key) records

get :: RecordSet -> String -> String
get (x:xs) k = if fst x == k then snd x else get xs k


input = "firstName = John\nlastName = Connor\nage = 30a"
tst = parsePerson input
