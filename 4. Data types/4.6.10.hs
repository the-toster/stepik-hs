import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
--    lookup k dict = lookup k (getListMap dict)
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)


instance MapLike ListMap where
    empty = ListMap []
    lookup k (ListMap []) = Nothing
    lookup k (ListMap ((key,v):xs)) = if k == key then Just v else lookup k $ ListMap xs

    insert k v dict = ListMap $ (k, v) : getListMap (delete k dict)

    delete k (ListMap xs) = ListMap $ foldr (\(key, v) b -> if k == key then b else (key, v) : b ) [] xs
