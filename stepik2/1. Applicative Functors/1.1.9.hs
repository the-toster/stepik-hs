-- map _ []     = []
-- map f (x:xs) = f x : map f xs

(1)
fmap (f . g) []                       -- def map
 == []
fmap f (fmap g [])                    -- def map
 == fmap f []                         -- def map
 == []                                -- =>
fmap (f . g) [] == fmap f (fmap g [])

(2)
fmap (f . g) (x:xs)                   -- def map
 == (f . g) x : fmap (f . g) xs

(3, IH: для xs утверждение верно)
fmap f (fmap g (x:xs))                -- def map
 == fmap f (g x : fmap g xs)          -- def map
 == f (g x) : fmap f (fmap g xs)      -- def (.)
 == (f . g) x : fmap f (fmap g xs)    -- IH
 == (f . g) x : fmap (f . g) xs       -- (2)
 == fmap (f . g) (x:xs)
