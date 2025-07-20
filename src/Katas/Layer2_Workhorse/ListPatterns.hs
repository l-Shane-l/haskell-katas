module Katas.Layer2_Workhorse.ListPatterns where

-- Goal: Practice common list processing patterns using idiomatic Haskell
-- (No if-then-else allowed! Use pattern matching and guards)

-- | Count elements in a list (practice basic recursion)
countElements :: [a] -> Int
countElements = undefined

{- | Get the last element of a list (practice pattern matching)
Hint: What's the last element of [x]? What about (x:xs)?
-}
lastElement :: [a] -> Maybe a
lastElement = undefined

{- | Take every nth element from a list
everyNth 2 [1,2,3,4,5,6] = [2,4,6]
-}
everyNth :: Int -> [a] -> [a]
everyNth = undefined

{- | Run-length encoding: compress consecutive duplicates
runLengthEncode "aaabbcaaa" = [(3,'a'),(2,'b'),(1,'c'),(3,'a')]
-}
runLengthEncode :: (Eq a) => [a] -> [(Int, a)]
runLengthEncode = undefined

{- | Find all positions where an element occurs
positions 'a' "abacad" = [0,2,4]
-}
positions :: (Eq a) => a -> [a] -> [Int]
positions = undefined

{- | Split a list into chunks of size n
chunksOf 3 [1..10] = [[1,2,3],[4,5,6],[7,8,9],[10]]
-}
chunksOf :: Int -> [a] -> [[a]]
chunksOf = undefined

{- | Apply a list of functions to a single value
applyAll [(*2), (+10), (`div` 3)] 9 = [18, 19, 3]
-}
applyAll :: [a -> b] -> a -> [b]
applyAll = undefined

{- | Count occurrences of each element
frequency "hello world" = [('h',1),('e',1),('l',3),('o',2),(' ',1),('w',1),('r',1),('d',1)]
Order doesn't matter, but no duplicates allowed
-}
frequency :: (Eq a) => [a] -> [(a, Int)]
frequency = undefined

{- | Find the longest consecutive sequence of an element
longestRun [1,1,2,2,2,3,3,3,3,2,2] = 4 (four 3s in a row)
-}
longestRun :: (Eq a) => [a] -> Int
longestRun = undefined

{- | Interleave two lists
interleave [1,2,3] ['a','b','c','d'] = [1,'a',2,'b',3,'c']
-}
interleave :: [a] -> [a] -> [a]
interleave = undefined
