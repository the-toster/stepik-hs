import Prelude hiding (lookup)
import qualified Data.List as Lib (lookup)
class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap (\ _ -> Nothing)
    lookup k (ArrowMap f) = f k
    insert k v (ArrowMap f) = ArrowMap (\x -> if x == k then Just v else f x)
    delete k (ArrowMap f) = ArrowMap (\x -> if x == k then Nothing else f x)
    fromList xs = ArrowMap (\k -> Lib.lookup k xs)

