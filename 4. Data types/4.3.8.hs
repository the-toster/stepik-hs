data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName p = p {firstName = abbr $ firstName p}


abbr (x:y:z) = x:"."
abbr x = x
