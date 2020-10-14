data List a = Cons a (List a)
            | Nil
              deriving (Show)

toList [] = Nil
toList (x:xs) = Cons x (toList xs)
