processData :: SomeData -> String
processData d = case doSomeWork d of
        (Success, _) -> "Success"
        (Fail, n)    -> "Fail: " ++ show n
