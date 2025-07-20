module Katas.Layer2_Workhorse.PatternMatching where

-- Goal: Master pattern matching - Haskell's most important feature
-- NO if-then-else allowed! Use patterns and guards only.

-- === LEVEL 1: Basic Patterns ===

-- | Describe a number as "zero", "one", "negative", or "positive"
describeNumber :: Int -> String
describeNumber = undefined

-- | Get the first element of a list safely
safeHead :: [a] -> Maybe a
safeHead = undefined

-- | Drop the first element safely (return empty list if already empty)
safeTail :: [a] -> [a]
safeTail = undefined

{- | Greet people, but give special greetings to specific names
"Alice" -> "Hey Alice, how's Bob?"
"Bob" -> "Yo Bob, how's Alice?"
anyone else -> "Hello, [name]!"
-}
greet :: String -> String
greet = undefined

-- === LEVEL 2: Tuple Patterns ===

{- | Extract age from different tuple formats
(name, age) -> age
(name, age, city) -> age
-}
getAge :: Either (String, Int) (String, Int, String) -> Int
getAge = undefined

{- | Swap tuple elements based on a condition
If sum > 10, swap them, otherwise keep as is
-}
swapIfBig :: (Int, Int) -> (Int, Int)
swapIfBig = undefined

{- | Calculate distance from origin
(0,0) -> "at origin"
(0,y) -> "on y-axis at [y]"
(x,0) -> "on x-axis at [x]"
(x,y) -> "at point ([x],[y])"
-}
describePoint :: (Int, Int) -> String
describePoint = undefined

-- === LEVEL 3: List Patterns ===

-- | Check if a list has exactly 2 elements
isListOfTwo :: [a] -> Bool
isListOfTwo = undefined

-- | Get the second element if it exists
secondElement :: [a] -> Maybe a
secondElement = undefined

-- | Sum the first 3 elements (or all if less than 3)
sumFirstThree :: (Num a) => [a] -> a
sumFirstThree = undefined

{- | Describe list by length
[] -> "empty"
[_] -> "singleton"
[_,_] -> "pair"
[_,_,_] -> "triple"
longer -> "list of many"
-}
describeList :: [a] -> String
describeList = undefined

-- === LEVEL 4: Nested Patterns ===

{- | Get the first element of the first list
[[1,2],[3,4]] -> Just 1
[[],[1]] -> Nothing
-}
firstOfFirst :: [[a]] -> Maybe a
firstOfFirst = undefined

{- | Check if list starts with a specific pair
startsWith 1 2 [1,2,3,4] -> True
-}
startsWith :: (Eq a) => a -> a -> [a] -> Bool
startsWith = undefined

{- | Extract nested tuple data
Just (Left (x, _)) -> x
Just (Right _) -> 0
Nothing -> -1
-}
extractNested :: Maybe (Either (Int, String) String) -> Int
extractNested = undefined

-- === LEVEL 5: As-Patterns (@) ===

{- | Show the list and its head
[1,2,3] -> "The list [1,2,3] starts with 1"
[] -> "The list is empty"
-}
describeListAndHead :: (Show a) => [a] -> String
describeListAndHead = undefined

{- | Keep the first occurrence of duplicate pairs
[(1,2), (1,3)] -> Just (1,2)
[(1,2), (2,3)] -> Nothing
-}
firstDuplicate :: (Eq a) => [(a, b)] -> Maybe (a, b)
firstDuplicate = undefined

-- === LEVEL 6: Guards with Patterns ===

{- | Classify lists by content and length
empty -> "empty"
single negative -> "single negative"
single positive -> "single positive"
all positive -> "all positive"
all negative -> "all negative"
otherwise -> "mixed"
-}
classifyIntList :: [Int] -> String
classifyIntList = undefined

{- | Process a Maybe Int with validation
Nothing -> "missing"
Just n where n < 0 -> "negative not allowed"
Just 0 -> "zero"
Just n where n > 100 -> "too large"
Just n -> "valid: [n]"
-}
validateInput :: Maybe Int -> String
validateInput = undefined

-- === LEVEL 7: Case Expressions ===

{- | Parse a simple command string
"quit" -> handle quit
"help" -> show help
"add [x] [y]" -> add the numbers
other -> error message
-}
parseCommand :: String -> String
parseCommand str = undefined

-- | Process nested Maybe values with different behaviors
processNested :: Maybe (Maybe Int) -> Int
processNested = undefined

-- === LEVEL 8: Complex Real-World Patterns ===

{- | Run-length decode
[(3,'a'), (2,'b'), (1,'c')] -> "aaabbc"
-}
runLengthDecode :: [(Int, a)] -> [a]
runLengthDecode = undefined

{- | Find the first Just value in a list
[Nothing, Nothing, Just 3, Just 4] -> Just 3
-}
firstJust :: [Maybe a] -> Maybe a
firstJust = undefined

{- | Zip with a custom combiner, but stop on specific condition
If either element is 0, stop zipping
zipUntilZero [1,2,0,4] [5,6,7,8] -> [(1,5), (2,6)]
-}
zipUntilZero :: (Eq a, Num a) => [a] -> [a] -> [(a, a)]
zipUntilZero = undefined

{- | Match on multiple arguments simultaneously
(0, _) -> "first is zero"
(_, 0) -> "second is zero"
(x, y) where x == y -> "equal"
(x, y) where x > y -> "first is larger"
otherwise -> "second is larger"
-}
compareTwo :: Int -> Int -> String
compareTwo = undefined

-- === CHALLENGE: Avoid Partial Functions ===

-- | These functions should handle ALL cases - no crashes!

-- Safe division
safeDiv :: Int -> Int -> Maybe Int
safeDiv = undefined

-- Get nth element safely
safeNth :: Int -> [a] -> Maybe a
safeNth = undefined

-- Parse a coordinate string: "(x,y)" -> Just (x,y)
-- Invalid format -> Nothing
parseCoordinate :: String -> Maybe (Int, Int)
parseCoordinate = undefined
