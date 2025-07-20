module Katas.Layer2_Workhorse.Chapter2Exercises where

import Data.List (foldl')
import Prelude hiding (concat, concatMap, reverse, zipWith)

-- === Exercise 1: Reversing a List with Folds ===

{- | Reverse a list using foldl
Think: What's accumulating? How does each element get added?
-}
reverseWithFoldl :: [a] -> [a]
reverseWithFoldl = undefined

{- | Reverse a list using foldr
This is trickier! Think about the order of operations
-}
reverseWithFoldr :: [a] -> [a]
reverseWithFoldr = undefined

{- | Which one is more natural? Which is more efficient?
Write a comment explaining your reasoning
-}
reverseAnalysis :: String
reverseAnalysis = "Your analysis here..."

-- === Exercise 2: Zipping Lists ===

{- | Implement zipWith using explicit recursion (no list comprehensions)
zipWith (+) [1,2,3] [4,5,6] = [5,7,9]
-}
zipWithRecursive :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithRecursive = undefined

{- | Implement zipWith using list comprehensions
Hint: This is tricky! You need indices
-}
zipWithComprehension :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithComprehension = undefined

{- | Can you implement zipWith using fold? If so, do it. If not, explain why.
This is a thought-provoking question!
-}
zipWithFold :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithFold = undefined

-- | Explain whether zipWith can be implemented with fold
zipWithFoldAnalysis :: String
zipWithFoldAnalysis = "Your analysis here..."

-- === Exercise 3: Implementing concatMap ===

-- | First, implement concat using foldr
concat :: [[a]] -> [a]
concat = undefined

-- | Implement concatMap using foldl
concatMapFoldl :: (a -> [b]) -> [a] -> [b]
concatMapFoldl = undefined

-- | Implement concatMap using foldr
concatMapFoldr :: (a -> [b]) -> [a] -> [b]
concatMapFoldr = undefined

-- | Which implementation is better and why?
concatMapAnalysis :: String
concatMapAnalysis = "Your analysis here..."

-- === Exercise 4: Thinking about Maps and Folds ===

-- | These two functions might behave the same sometimes
mapThenFold :: (a -> b) -> (b -> c -> c) -> c -> [a] -> c
mapThenFold f g z xs = foldr g z (map f xs)

foldWithCompose :: (a -> b) -> (b -> c -> c) -> c -> [a] -> c
foldWithCompose f g z xs = foldr (g . f) z xs

-- | Implement test cases showing when they're the same
sameBehaviorExample :: Bool
sameBehaviorExample = undefined -- Should return True

-- | Implement test cases showing when they differ
differentBehaviorExample :: Bool
differentBehaviorExample = undefined -- Should return True

-- | Explain the difference
mapFoldAnalysis :: String
mapFoldAnalysis = "Your analysis here..."

-- === Exercise 5: Folds and Infinite Lists ===

{- | Try to trace through foldl on an infinite list manually
What happens? Why can't it work?
-}
foldlInfiniteAnalysis :: String
foldlInfiniteAnalysis = "Your analysis here..."

{- | Implement a function using foldr that works on infinite lists
Example: Find first element > 100 in an infinite list
-}
findFirstGreater :: (Ord a) => a -> [a] -> Maybe a
findFirstGreater = undefined

{- | Are there benefits to foldr on large FINITE lists?
Implement a function that shows early termination
-}
earlyTermination :: [Int] -> Bool
earlyTermination = undefined -- Should check if any element > 1000

-- === BONUS: Fibonacci Variations ===

-- | Implement fibs using the "tying the knot" technique
fibs :: [Integer]
fibs = undefined

-- | Implement a parameterized Fibonacci that starts with any two numbers
fibsFrom :: Integer -> Integer -> [Integer]
fibsFrom = undefined

{- | Create an infinite list of Fibonacci ratios (golden ratio approximation)
Each ratio is: fib(n+1) / fib(n)
-}
goldenRatios :: [Double]
goldenRatios = undefined

-- === CHALLENGE: Stream Processing ===

{- | Implement the Hamming numbers (numbers of form 2^a * 3^b * 5^c)
This is a classic problem that really tests understanding of lazy streams
-}
hamming :: [Integer]
hamming = undefined

{- | Implement a function that merges sorted infinite streams
(needed for Hamming numbers)
-}
mergeSorted :: (Ord a) => [a] -> [a] -> [a]
mergeSorted = undefined
