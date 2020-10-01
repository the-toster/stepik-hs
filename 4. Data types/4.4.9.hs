import Data.List.Split (splitOn)
import Data.Char (isDigit)
data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int }

parsePerson :: String -> Either Error Person
parsePerson = undefined

data PersonField = Name String | Age Int deriving Show



pairs inp = map makePair $ items inp where
    makePair [k,v] = makeField k v
        where
            makeField "age" v = if all isDigit v then Right (k, read v) else Left IncorrectDataError v
            makeField k v = Right (k, v)
    makePair _ = Left ParsingError

items inp = map (splitOn " = ") $ lines inp

input = "\nwer = \nfirstName = John\nlastName = Connor\nage = 30"
t = pairs input
