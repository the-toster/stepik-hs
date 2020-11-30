import Control.Applicative

newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP predicate = PrsEP f where
    f pos "" = (newPos, Left ("pos "++ (show newPos) ++ ": unexpected end of input"))
        where newPos = pos + 1
    f pos (x:xs) = (newPos,
                        if predicate x then Right (x, xs) else
                            Left ("pos "++ (show newPos) ++ ": unexpected " ++ [x])
                        )
        where newPos = pos + 1

charEP c = satisfyEP (== c)
anyEP = satisfyEP (const True)
testP = (,) <$> anyEP <* charEP 'B' <*> anyEP

tst = [
        (runPrsEP (pure 42) 0 "ABCDEFG") == (0,Right (42,"ABCDEFG")),
        (runPrsEP testP 0 "ABCDE") == (3,Right (('A','C'),"DE")),
        (parseEP testP "BCDE") == (Left "pos 2: unexpected C"),
        (parseEP testP "") == (Left "pos 1: unexpected end of input"),
        (parseEP testP "B") == (Left "pos 2: unexpected end of input")
        ]

instance Functor PrsEP where
    fmap f (PrsEP g) = PrsEP h where
        h pos s =  fmap ( fmap (\(x, xs) -> (f x, xs)) )  (g pos s)

instance Applicative PrsEP where
    pure a = PrsEP (\pos s -> (pos, Right (a, s)))
    (PrsEP pf) <*> (PrsEP pv) = PrsEP fv where
        fv pos s = case pf pos s of
            (p1, Left s) -> (p1, Left s)
            (p1, Right (x, xs)) -> case pv p1 xs of
                (p2, Left s) -> (p2, Left s)
                (p2, Right (y, ys)) -> (p2, Right (x y, ys))



testParser = (charEP 'A')
mappedParser = fmap (\_ -> 'D') testParser

