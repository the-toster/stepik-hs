newtype Reader r a = Reader { runReader :: (r -> a) }

instance Monad (Reader r) where
    return x = Reader $ \e -> x
    m >>= k  = Reader $ \e ->
                let  v = runReader m e
                in runReader (k v) e
type User = String
type Password = String
type UsersTable = [(User, Password)]

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = do
    lst <- ask
    return $ map (fst) $ filter (\(l, p) -> p == "123456") lst

ask :: Reader r r
ask = Reader id

tst = runReader usersWithBadPasswords [("user", "123456"), ("x", "hi"), ("root", "123456")] == ["user","root"]
