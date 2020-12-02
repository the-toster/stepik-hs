import Control.Monad.Trans.Except

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex
  deriving (Eq, Show)


infixl 9 !!!
(!!!) :: [a] -> Int -> Except ListIndexError a
(!!!) xs i = if i < 0
     then except $ Left ErrNegativeIndex
     else if xs `shorterThan` i
         then except $ Left $ ErrIndexTooLarge i
         else except $ Right $ xs !! i

shorterThan xs n = null $ drop n xs


(!!!!) xs n = runExcept $ xs !!! n
tst = [
    (runExcept $ [1..100] !!! 5) == (Right 6),
    ([1,2,3] !!!! 0) == (Right 1),
    ([1,2,3] !!!! 42) == (Left (ErrIndexTooLarge 42)),
    ([1,2,3] !!!! (-3)) == (Left ErrNegativeIndex)
    ]
