module Stream where

import Control.Arrow
import Control.Applicative

import Stream.Internal

-- Defined in Stream.Internal:
--     data Stream a = a :> Stream a
--     infixr :>

-- | Get the first element of a stream.
headS :: Stream a -> a
headS = error "headS: not yet implemented"

-- | Drop the first element of a stream.
tailS :: Stream a -> Stream a
tailS = error "tailS: not yet implemented"


-- {{{ Stream constructors

-- | Construct a stream by repeating a value.
repeatS :: a -> Stream a
repeatS = error "repeatS: not yet implemented"

-- | Construct a stream by repeatedly applying a function.
iterateS :: (a -> a) -> a -> Stream a
iterateS f x = error "iterateS: not yet implemented"

-- | Construct a stream by repeating a list forever.
cycleS :: [a] -> Stream a
cycleS xs = error "cycleS: not yet implemented"

-- | Construct a stream by counting numbers starting from a given one.
fromS :: Num a => a -> Stream a
fromS = error "fromS: not yet implemented"

-- | Same as 'fromS', but count with a given step width.
fromStepS :: Num a => a -> a -> Stream a
fromStepS x s = error "fromStepS: not yet implemented"

-- }}}


-- | Fold a stream from the left.
foldrS :: (a -> b -> b) -> Stream a -> b
foldrS f (x :> xs) = error "foldrS: not yet implemented"

-- | Filter a stream with a predicate.
filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p = error "filterS: not yet implemented"

-- | Take a given amount of elements from a stream.
takeS :: Int -> Stream a -> [a]
takeS i s = error "takeS: not yet implemented"

-- | Drop a given amount of elements from a stream.
dropS :: Int -> Stream a -> Stream a
dropS i s = error "dropS: not yet implemented"

-- | Do take and drop simultaneous.
splitAtS :: Int -> Stream a -> ([a], Stream a)
splitAtS i s = error "splitS: not yet implemented"

-- | Combine two streams with a function.
zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS f xs ys = error "zipWithS: not yet implemented"

zipS :: Stream a -> Stream b -> Stream (a, b)
zipS = zipWithS (,)

instance Functor Stream where
    -- fmap :: (a -> b) -> Stream a -> Stream b
    fmap f (x :> xs) = error "fmap: not yet implemented"

instance Applicative Stream where
    -- pure :: a -> Stream a
    pure = error "pure: not yet implemented"

    -- (<*>) :: Stream (a -> b) -> Stream a -> Stream b
    (<*>) = error "(<*>): not yet implemented"

-- | The stream of fibonacci numbers.
fibS :: Stream Integer
fibS = error "fibS: not yet implemented"

-- | The stream of prime numbers.
primeS :: Stream Integer
primeS = error "primeS: not yet implemented"
