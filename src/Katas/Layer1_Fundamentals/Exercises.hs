module Katas.Layer1_Fundamentals.Exercises where

{- | FizzBuzz using recursion with accumulator
Given a number n, return a string with all numbers from 1 to n,
replacing multiples of 3 with "fizz", multiples of 5 with "buzz",
and multiples of both with "fizzbuzz"
-}
fizzBuzz :: Int -> String
fizzBuzz n = undefined

-- | Helper function for FizzBuzz - determines what a single number becomes
fizzBuzzFor :: Int -> String
fizzBuzzFor n = undefined

{- | Factorial function
Calculate n! = n * (n-1) * (n-2) * ... * 1
factorial 0 = 1 (by definition)
-}
factorial :: Integer -> Integer
factorial n = undefined

{- | Fibonacci sequence
Calculate the nth Fibonacci number
The sequence goes: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34...
Each number is the sum of the two preceding ones
-}
fibonacci :: Integer -> Integer
fibonacci n = undefined

{- | Manual curry implementation
Convert a function that takes a tuple into a function that takes two arguments
-}
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f = undefined

{- | Manual uncurry implementation
Convert a function that takes two arguments into a function that takes a tuple
-}
myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f = undefined

-- Additional helper exercises to practice the recursion pattern:

{- | Sum numbers from 1 to n using recursion with accumulator
sumTo 5 = 1 + 2 + 3 + 4 + 5 = 15
-}
sumTo :: Int -> Int
sumTo n = undefined

{- | Count down from n to 1, returning a string
countDown 5 = "5 4 3 2 1 Done!"
-}
countDown :: Int -> String
countDown n = undefined

{- | Double a number repeatedly until it exceeds the target
doubleUntil 10 starting from 1 = 16 (1 -> 2 -> 4 -> 8 -> 16)
-}
doubleUntil :: Int -> Int
doubleUntil target = undefined
