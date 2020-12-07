import Control.Monad.Trans.Except
import Data.Foldable
import Text.Read

data ReadError = EmptyInput | NoParse String
  deriving (Show, Eq)

data SumError = SumError Int ReadError
  deriving (Show, Eq)

newtype Validate e a = Validate { getValidate :: Either [e] a } deriving (Show, Eq)

instance Num a => Semigroup (Validate e a) where
    (<>) = mappend

instance Num a => Monoid (Validate e a) where
    mempty = Validate $ Right 0
    v1 `mappend` v2 = case (getValidate v1, getValidate v2) of
        (Left e1, Left e2) -> Validate $ Left (e1 ++ e2)
        (_      , Left e)  -> Validate $ Left e
        (Left e, _)        -> Validate $ Left e
        (Right s1, Right s2) -> Validate $ Right $ s1 + s2

tst = [
        (getValidate $ validateSum ["10", "20", "30"]) == Right 60,
        (getValidate $ validateSum ["10", "", "30", "oops"]) == Left [SumError 2 EmptyInput,SumError 4 (NoParse "oops")]
    ]


collectE :: Except e a -> Validate e a
collectE e = case runExcept e of
    Left e  -> Validate $ Left [e]
    Right a -> Validate $ Right a

validateSum :: [String] -> Validate SumError Integer
validateSum xs = foldMap collectE $ tryReadInts xs


tryReadInts :: [String] -> [Except SumError Integer]
tryReadInts xs = fmap f $ zip [1..] (fmap tryRead xs) where
    f (n, x) = case runExcept x of
        Left e -> except $ Left $ SumError n e
        Right a -> except $ Right a

tryRead :: Read a => String -> Except ReadError a
tryRead "" = except $ Left EmptyInput
tryRead s = case readMaybe s of
    Nothing -> except $ Left $ NoParse s
    Just a -> except $ Right a
