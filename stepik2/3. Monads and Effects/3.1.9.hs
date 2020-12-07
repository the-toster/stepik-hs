import Control.Monad.Trans.Except

import Text.Read

data ReadError = EmptyInput | NoParse String
  deriving (Show, Eq)

tryRead :: Read a => String -> Except ReadError a
tryRead "" = except $ Left EmptyInput
tryRead s = case readMaybe s of
    Nothing -> except $ Left $ NoParse s
    Just a -> except $ Right a

data SumError = SumError Int ReadError
  deriving (Show, Eq)

tst = [
    (runExcept $ trySum ["10", "20", "30"]) == Right 60,
    (runExcept $ trySum ["10", "20", ""]) == Left (SumError 3 EmptyInput),
    (runExcept $ trySum ["10", "two", "30"]) == Left (SumError 2 (NoParse "two"))
    ]

trySum :: [String] -> Except SumError Int
trySum xs = except $ foldl f (Right 0) $ zip [1..] xs where
    f (Left e) _ = Left e
    f (Right acc) (n, s) = case readInt s of
        Left e -> Left $ SumError n e
        Right v -> Right $ acc + v

    readInt :: String -> Either ReadError Int
    readInt s = runExcept $ tryRead s
