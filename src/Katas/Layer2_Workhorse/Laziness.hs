module Katas.Layer2_Workhorse.Laziness where

import Data.List (foldl')

-- Goal: Understand when laziness helps and when it hurts

-- === PART 1: Space Leaks ===

{- | Calculate sum of squares - this has a space leak!
Fix it to use constant memory
-}
sumSquaresBad :: [Int] -> Int
sumSquaresBad xs = foldl (\acc x -> acc + x * x) 0 xs

sumSquaresGood :: [Int] -> Int
sumSquaresGood = undefined

{- | Calculate both sum and product - this builds TWO thunk chains!
Rewrite to traverse the list only once with strict accumulation
-}
sumAndProductBad :: [Double] -> (Double, Double)
sumAndProductBad xs = (sum xs, product xs)

sumAndProductGood :: [Double] -> (Double, Double)
sumAndProductGood = undefined

-- === PART 2: Infinite Lists ===

-- | Generate infinite list of powers of 2: [1,2,4,8,16,...]
powersOfTwo :: [Integer]
powersOfTwo = undefined

-- | Find first power of 2 greater than n
firstPowerGreaterThan :: Integer -> Integer
firstPowerGreaterThan = undefined

{- | Generate Fibonacci numbers as an infinite list
Use the "tying the knot" technique
-}
fibs :: [Integer]
fibs = undefined

-- === PART 3: Lazy vs Strict Trade-offs ===

{- | Take the first n elements that satisfy a predicate
Should work on infinite lists!
-}
takeWhileN :: Int -> (a -> Bool) -> [a] -> [a]
takeWhileN = undefined

-- | Find the nth prime number (use infinite list of primes)
nthPrime :: Int -> Integer
nthPrime = undefined

{- | Process a "file" (list) line by line, stopping when we find "STOP"
Should work lazily - not process lines after "STOP"
-}
processUntilStop :: [String] -> [String]
processUntilStop = undefined

-- === PART 4: Understanding Evaluation ===

{- | Which of these will evaluate? Which will crash?
Fix the ones that crash
-}
example1 = length [undefined, undefined, undefined] -- What happens?

example2 = head [undefined, 1, 2] -- What happens?
example3 = tail [1, undefined, 3] -- What happens?
example4 = take 2 [1, 2, undefined] -- What happens?

{- | Create a function that returns the first n elements
but crashes if you try to access element n+1
-}
dangerousTake :: Int -> [a] -> [a]
dangerousTake = undefined

-- === PART 5: Practical Laziness ===

{- | Generate all Pythagorean triples (a,b,c) where a² + b² = c²
Should produce them in increasing order of a+b+c
-}
pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = undefined

{- | Merge two sorted infinite lists into one sorted list
e.g., merge [1,3,5...] [2,4,6...] = [1,2,3,4,5,6...]
-}
mergeSorted :: (Ord a) => [a] -> [a] -> [a]
mergeSorted = undefined

{- | The Hamming sequence: all numbers of form 2^a * 3^b * 5^c
in ascending order: [1,2,3,4,5,6,8,9,10,12,15,16,18,20,...]
This is a classic lazy evaluation problem!
-}
hamming :: [Integer]
hamming = undefined
