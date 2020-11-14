{-# LANGUAGE FlexibleInstances,
    TypeFamilies,
    FunctionalDependencies,
    UndecidableInstances
    #-}

module PolyvariadicFunctions where

-- `polyList` turns its arguments into a list, polymorphically.
class PolyListType a t | a -> t where
    polyListT :: a -> t

instance PolyListType [a] [a] where
    polyListT acc = acc

instance PolyListType a (a -> r) => PolyListType a (a -> r) where
    polyListT acc = \x -> polyListT $ acc ++ [x]

polyList :: (PolyListType a t) => t
polyList = polyListT []




-- `polyAdd` sums its arguments, all `Int`s.
class PolyAddType t where
    polyAddT :: Int -> t

instance PolyAddType Int where
    polyAddT acc = acc

instance (a ~ Int, PolyAddType r) => PolyAddType (a -> r) where
    polyAddT acc = \x -> polyAddT (acc + x)

polyAdd :: (PolyAddType t) => t
polyAdd = polyAddT 0

---- `polyWords` turns its arguments into a spaced string.

class PolyWordsType t where
    polyWordsT :: [String] -> t

instance PolyWordsType String where
    polyWordsT acc = unwords $ reverse acc

instance (PolyWordsType r) => PolyWordsType (String -> r) where
    polyWordsT acc = \x -> polyWordsT $ x : acc

polyWords :: (PolyWordsType t) => t
polyWords = polyWordsT []

