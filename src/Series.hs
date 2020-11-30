module Series
  ( fibonacci,
    primes,
    primesSlow,
    naturals,
    evens,
    odds,
    factorials,
    sums,
    products,
  )
where

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

factorials :: [Integer]
factorials = 1 : zipWith (*) factorials [1 ..]

products :: [Integer]
products = factorials

sums :: [Integer]
sums = 0 : zipWith (+) sums [1 ..]

-- Of course we could just use [0 ..], but this is
-- much more illustrutive of haskell's lazy evaluation
-- and infinite lists.
naturals :: [Integer]
naturals = 0 : map (+ 1) naturals

evens :: [Integer]
evens = filter even naturals

odds :: [Integer]
odds = filter odd naturals

-- This is not the most efficient implementation,
-- but it's easier to understand than the one below.
primesSlow :: [Integer]
primesSlow = sieve (2 : tail odds)
  where
    sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

primes :: [Integer]
primes = 2 : 3 : calcNextPrimes (tail primes) [5, 7 ..]
  where
    calcNextPrimes (p : ps) candidates =
      let (mustBePrimes, _ : potentialPrimes) = span (< p * p) candidates
       in mustBePrimes ++ calcNextPrimes ps [x | x <- potentialPrimes, rem x p /= 0]
