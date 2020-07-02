import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
    -- empty :: ListMap k v
    empty = ListMap []

    -- lookup :: Ord k => k -> ListMap k v -> Maybe v
    lookup k (ListMap m) = L.find (\(mk, mv) -> mk == k) m >>= Just . snd

    -- insert :: Ord k => k -> v -> ListMap k v -> ListMap k v
    insert k v (ListMap m) = ListMap $ left ++ (k, v) : right' where
        (left, right) = span (\(mk, mv) -> mk /= k) m
        right' = case right of
            [] -> []
            x:xs -> xs

    -- delete :: Ord k => k -> ListMap k v -> ListMap k v
    delete k (ListMap m) = ListMap $ filter (\(mk, mv) -> mk /= k) m

    -- fromList :: Ord k => [(k, v)] -> ListMap k v
    fromList = ListMap


newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap $ const Nothing

    lookup k am = getArrowMap am k

    insert k v am = ArrowMap f' where
        f' k' | k' == k = Just v
              | otherwise = getArrowMap am k'

    delete k am = ArrowMap f' where
        f' k' | k' == k =  Nothing
              | otherwise = getArrowMap am k'

    fromList = foldl (\am (k, v) -> insert k v am) empty
