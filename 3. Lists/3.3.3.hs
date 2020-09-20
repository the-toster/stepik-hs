{-
Реализуйте c использованием функции zipWith функцию fibStream, возвращающую бесконечный список чисел Фибоначчи.
-}

fibStream :: [Integer]
fibStream = 0 : calc [0, 1] where
        calc xs = next : calc (next:xs) where
            next = head $ zipWith (+) xs (tail xs)

test = (take 10 $ fibStream) == [0,1,1,2,3,5,8,13,21,34]




{-
а правильный ответ такой:
fibStream = 0 : zipWith (+) fibStream (1 : fibStream)

...


это печально))


-}
