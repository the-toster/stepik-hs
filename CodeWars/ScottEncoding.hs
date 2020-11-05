{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

toPair :: SPair a b -> (a,b)
toPair p = (fst p, snd p)
fromPair :: (a,b) -> SPair a b
fromPair (a, b) = SPair $ \f -> f a b
fst :: SPair a b -> a
fst p = runPair p (\a _ -> a)
snd :: SPair a b -> b
snd p = runPair p (\_ b -> b)
swap :: SPair a b -> SPair b a
swap p = let (a, b) = toPair p in fromPair (b, a)
curry :: (SPair a b -> c) -> (a -> b -> c)
curry f = \a b -> f (fromPair (a, b))
uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f = \p -> f (fst p) (snd p)

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
toMaybe :: SMaybe a -> Maybe a
toMaybe m = runMaybe m Nothing Just
fromMaybe :: Maybe a -> SMaybe a
fromMaybe (Just x) = SMaybe $ \_ f -> f x
fromMaybe Nothing  = SMaybe $ \x _ -> x
isJust :: SMaybe a -> Bool
isJust m = runMaybe m False (const True)
isNothing :: SMaybe a -> Bool
isNothing = not . isJust
catMaybes :: SList (SMaybe a) -> SList a
catMaybes = error "catMaybes"

newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
toEither :: SEither a b -> Either a b
toEither e = runEither e Left Right
fromEither :: Either a b -> SEither a b
fromEither (Left a) = SEither $ \f _ -> f a
fromEither (Right b) = SEither $ \_ f -> f b
isLeft :: SEither a b -> Bool
isLeft e = runEither e (const True) (const False)
isRight :: SEither a b -> Bool
isRight = not . isLeft
partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition = error "partition"

newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
toList :: SList a -> [a]
toList = error "toList"
fromList :: [a] -> SList a
fromList []     = SList const
fromList (x:xs) = SList $ \_ f -> f x (fromList xs)
cons :: a -> SList a -> SList a
cons = error "cons"
concat :: SList a -> SList a -> SList a
concat = error "concat"
null :: SList a -> Bool
null = error "null"
length :: SList a -> Int
length = error "length"
map :: (a -> b) -> SList a -> SList b
map = error "map"
zip :: SList a -> SList b -> SList (SPair a b)
zip = error "zip"
foldl :: (b -> a -> b) -> b -> SList a -> b
foldl = error "foldl"
foldr :: (a -> b -> b) -> b -> SList a -> b
foldr = error "foldr"
take :: Int -> SList a -> SList a
take = error "take"
