data Triple a = Tr a a a  deriving (Eq,Show)

instance Foldable Triple where
    foldr f ini (Tr a b c) = foldr f ini [a, b, c]
    foldl f ini (Tr a b c) = foldl f ini [a, b, c]
