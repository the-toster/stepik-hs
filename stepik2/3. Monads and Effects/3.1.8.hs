import Text.Read

tryRead :: Read a => String -> Except ReadError a
tryRead "" = except $ Left EmptyInput
tryRead s = case readMaybe s of
    Nothing -> except $ Left $ NoParse s
    Just a -> except $ Right a
