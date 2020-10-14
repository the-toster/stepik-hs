data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)


height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)

tst = [
    height Empty == 0,
    height (Node "x" Empty (Node "y" Empty Empty))  == 2
    ]
