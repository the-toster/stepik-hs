{-# LANGUAGE GADTs, DataKinds,
             TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module OddsAndEvens where

-- | The natural numbers.
data Nat = Z | S Nat

-- | The axioms of even numbers.
data Even (a :: Nat) :: * where
  -- | Zero is even.
  ZeroEven :: Even Z
  -- | If n is even, then n+2 is even.
  NextEven :: Even n -> Even (S (S n))

-- | The axioms of odd numbers.
data Odd (a :: Nat) :: * where
  -- | One is odd.
  OneOdd :: Odd (S Z)
  -- | If n is odd, then n+2 is odd.
  NextOdd :: Odd n -> Odd (S (S n))

-- | Proves that if n is even, n+1 is odd.
-- Notice how I use the axioms here.
evenPlusOne :: Even n -> Odd (S n)
evenPlusOne ZeroEven = OneOdd
evenPlusOne (NextEven n) = NextOdd (evenPlusOne n)

-- | Proves that if n is odd, n+1 is even.
oddPlusOne :: Odd n -> Even (S n)
oddPlusOne OneOdd =  NextEven ZeroEven
oddPlusOne (NextOdd n) =  NextEven (oddPlusOne n)

-- | Adds two natural numbers together.
-- Notice how the definition pattern matches.
type family   Add (n :: Nat) (m :: Nat) :: Nat
type instance Add Z m = m
type instance Add (S n) m = S (Add n m)

-- | Proves even + even = even
-- Notice how the pattern matching mirrors `Add`s definition.
evenPlusEven :: Even n -> Even m -> Even (Add n m)
evenPlusEven ZeroEven m = m
evenPlusEven (NextEven n) m = NextEven (evenPlusEven n m)

-- | Proves odd + odd = even
oddPlusOdd :: Odd n -> Odd m -> Even (Add n m)
oddPlusOdd OneOdd m = oddPlusOne m
oddPlusOdd (NextOdd n) m = NextEven (oddPlusOdd n m)

-- | Proves even + odd = odd
evenPlusOdd :: Even n -> Odd m -> Odd (Add n m)
evenPlusOdd ZeroEven m = m
evenPlusOdd (NextEven n) m = NextOdd (evenPlusOdd n m)

-- | Proves odd + even = odd
oddPlusEven :: Odd n -> Even m -> Odd (Add n m)
oddPlusEven OneOdd m = evenPlusOne m
oddPlusEven (NextOdd n) m = NextOdd (oddPlusEven n m)

-- | Multiplies two natural numbers.
type family   Mult (n :: Nat) (m :: Nat) :: Nat
type instance Mult Z m = Z
type instance Mult (S n) m = Add (Mult n m) m

-- | Proves even * even = even
evenTimesEven :: Even n -> Even m -> Even (Mult n m)
evenTimesEven ZeroEven n = ZeroEven
evenTimesEven (NextEven n) m = evenPlusEven (evenPlusEven (evenTimesEven n m) m) m

-- | Proves odd * odd = odd
oddTimesOdd :: Odd n -> Odd m -> Odd (Mult n m)
oddTimesOdd OneOdd n  = n
oddTimesOdd (NextOdd n) m  =  evenPlusOdd (oddPlusOdd (oddTimesOdd n m) m) m

-- | Proves even * odd = even
evenTimesOdd :: Even n -> Odd m -> Even (Mult n m)
evenTimesOdd ZeroEven m = ZeroEven
evenTimesOdd (NextEven n) m = oddPlusOdd (evenPlusOdd (evenTimesOdd n m) m) m

---- | Proves odd * even = even
oddTimesEven :: Odd n -> Even m -> Even (Mult n m)
oddTimesEven OneOdd m = m
oddTimesEven (NextOdd n) m = evenPlusEven (evenPlusEven (oddTimesEven n m) m) m


-- Representations to Integers
fromEven :: Even n -> Int
fromEven ZeroEven = 0
fromEven (NextEven n) = 2 + fromEven n
fromOdd :: Odd n -> Int
fromOdd OneOdd = 1
fromOdd (NextOdd n) = 2 + fromOdd n

-- Numbers for help during tests
--zero = ZeroEven
--one = OneOdd
--two = NextEven zero
--three = NextOdd one
--four = NextEven two
--five = NextOdd three
--six = NextEven four
--seven = NextOdd five
--eight = NextEven six
--nine = NextOdd seven
--ten = NextEven eight
