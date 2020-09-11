{-
что такое uncurry (flip const)

uncurry :: (a -> b -> c) -> (a, b) -> c
flip const :: b -> c -> c

uncurry (flip const) :: (b, c) -> c

snd :: (a, b) -> c
-}
