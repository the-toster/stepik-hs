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
toList (SList l) = l [] (\a lst -> a : toList lst)
fromList :: [a] -> SList a
fromList []     = SList const
fromList (x:xs) = SList $ \_ f -> f x (fromList xs)
cons :: a -> SList a -> SList a
cons x xs = SList $ \_ f -> f x xs

concat :: SList a -> SList a -> SList a
concat l1 l2 = runList l1 l2 $ \a xs -> cons a (concat xs l2)

null :: SList a -> Bool
null (SList xs) = xs True (\_ _ -> False)

length :: SList a -> Int

length l = runList l 0 (\a xs -> 1 + length xs)

map :: (a -> b) -> SList a -> SList b
map f l = runList l (SList const) $ \a xs -> cons (f a) (map f xs)

zip :: SList a -> SList b -> SList (SPair a b)
zip l1 l2 = case zipHead l1 l2 of
                Nothing -> (SList const)
                Just (p, xs, ys) -> p `cons` zip xs ys
                where zipHead l1 l2 = do
                        (x, xs) <- unconsL l1
                        (y, ys) <- unconsL l2
                        return (SPair $ \f -> f x y, xs, ys)

unconsL l = runList l Nothing (\x xs -> Just (x, xs))

foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f ini l = case unconsL l of
                    Nothing -> ini
                    Just (x, xs) -> foldl f (f ini x) xs

foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f = foldl (flip f)

take :: Int -> SList a -> SList a
take i l | i < 1 = SList const
         | otherwise = runList l (SList const) $ \x xs -> x `cons` (take (i - 1) xs)




reduce :: Num a => a -> SList a -> a
reduce i sl = i + 10 * runList sl 0 reduce
