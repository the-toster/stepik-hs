newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

charEP c = satisfyEP (== c)

tst = [
    (runPrsEP (charEP 'A') 0 "ABC")  == (1,Right ('A',"BC")),
    (runPrsEP (charEP 'A') 41 "BCD") == (42,Left "pos 42: unexpected B"),
    (runPrsEP (charEP 'A') 41 "") == (42,Left "pos 42: unexpected end of input"),
    (parseEP (charEP 'A') "ABC") == Right ('A',"BC"),
    (parseEP (charEP 'A') "BCD") == Left "pos 1: unexpected B",
    (parseEP (charEP 'A') "") == Left "pos 1: unexpected end of input"
    ]


satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP predicate = PrsEP f where
    f pos "" = (newPos, Left ("pos "++ (show newPos) ++ ": unexpected end of input"))
        where newPos = pos + 1
    f pos (x:xs) = (newPos,
                        if predicate x then Right (x, xs) else
                            Left ("pos "++ (show newPos) ++ ": unexpected " ++ [x])
                        )
        where newPos = pos + 1
