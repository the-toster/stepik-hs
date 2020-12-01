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
tripleP [a,b,c] = (\x y z -> [x,y,z]) <$> charEP a <*> charEP b <*>  charEP c

tst = [
--        (runPrsEP empty 0 "ABCDEFG") == (0,Left "pos 0: empty alternative")
        (parseEP (tripleP "ABC" <|> tripleP "ADC") "ABE") == (Left "pos 3: unexpected E"),
        (parseEP (tripleP "ABC" <|> tripleP "ADC") "ADE") == (Left "pos 3: unexpected E"),
        (parseEP (tripleP "ABC" <|> tripleP "ADC") "AEF") == (Left "pos 2: unexpected E")
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

instance Alternative PrsEP where
    empty = PrsEP f where
        f pos _ = (pos, Left $ "pos " ++ show pos ++ ": empty alternative")

    p <|> q = PrsEP f where
        f p0 s = let
            (p1, r1) = runPrsEP p p0 s
            (p2, r2) = runPrsEP q p0 s
            in case r1 of
                Right (x, xs) -> (p1, Right (x, xs))
                Left s1 -> case r2 of
                     Right (x, xs) -> (p2, Right (x, xs))
                     Left s2 -> if p1 > p2 then
                                        (p1, Left s1) else
                                        (p2, Left s2)
