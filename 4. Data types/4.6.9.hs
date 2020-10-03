import Data.Monoid

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = (Maybe' (Just mempty))
    mappend (Maybe' (Just a)) (Maybe' (Just b)) = Maybe' (Just (mappend a b))
    mappend _ _ = Maybe' Nothing -- (вариант 2)
--    почему не так (вариант 1):
--    mappend (Maybe' Nothing ) (Maybe' (Just b)) = Maybe' (Just b)
--    mappend (Maybe' (Just a)) (Maybe' Nothing ) = Maybe' (Just a)
--    mappend (Maybe' Nothing ) (Maybe' Nothing ) = Maybe' Nothing
--    я сначала не понял, а потом появилась такая мысль - т.к. я реализую моноид на основе обернутого типа 'a', то,
--    т.к. Nothing не является членом этого типа, то и в mappend с ним не может участвовать, и семантика Maybe сохранится
--    но все равно, вроде законы не нарушаются и при варианте 1


test0 = if (mempty :: Maybe' [Int]) == Maybe' Nothing then "failed" else "passed"
