satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE p = PrsE f where
  f "" = Left "unexpected end of input"
  f (c:cs) = if p c then Right (c, cs) else Left ("unexpected " ++ [c])

charE :: Char -> PrsE Char
charE c = satisfyE (== c)
