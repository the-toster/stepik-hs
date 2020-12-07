import Control.Monad.Trans.Except
import Data.Foldable (msum)

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

newtype SimpleError = Simple { getSimple :: String }
  deriving (Eq, Show)

instance Semigroup SimpleError where
    (<>) = mappend

instance Monoid SimpleError where
    mempty = Simple ""
    mappend (Simple a) (Simple b) = Simple $ a ++ b

toSimpleFromList = runExcept . msum . map (withExcept lie2se)
toSimple = runExcept . withExcept lie2se
xs = [1,2,3]

tst = [
        (toSimple $ xs !!! 42) == Left (Simple {getSimple = "[index (42) is too large]"}),
        (toSimple $ xs !!! (-2)) == Left (Simple {getSimple = "[negative index]"}),
        (toSimple $ xs !!! 2) == Right 3,
        (toSimpleFromList [xs !!! (-2), xs !!! 42]) == Left (Simple {getSimple = "[negative index][index (42) is too large]"}),
        (toSimpleFromList [xs !!! (-2), xs !!! 2]) == Right 3
    ]

lie2se :: ListIndexError -> SimpleError
lie2se (ErrIndexTooLarge i) = Simple $ "[index (" ++ show i ++ ") is too large]"
lie2se ErrNegativeIndex = Simple "[negative index]"
