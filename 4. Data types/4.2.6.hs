data SomeData = D Int deriving (Show, Read)

data Result = Fail | Success

doSomeWork :: SomeData -> (Result,Int)
doSomeWork (D n) = (Fail, n)

data Result' = F Int | S

instance Show Result' where
    show (S) = "Success"
    show (F n)  = "Fail: " ++ show n

doSomeWork' :: SomeData -> Result'
doSomeWork' x = case doSomeWork x of
                    (Success, _) -> S
                    (Fail, n) -> F n
