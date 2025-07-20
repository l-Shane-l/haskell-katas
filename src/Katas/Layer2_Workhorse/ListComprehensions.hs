module Katas.Layer2_Workhorse.ListComprehensions where

-- Goal: Build muscle memory for list comprehensions
-- Start simple, build complexity gradually

-- === LEVEL 1: Basic Transformations ===
-- Rewrite these using list comprehensions

-- | Double all numbers: map (*2) [1..10]
doubles :: [Int]
doubles = undefined

-- | Keep only evens: filter even [1..20]
evens :: [Int]
evens = undefined

-- | Square all odd numbers: map (^2) . filter odd $ [1..10]
squaredOdds :: [Int]
squaredOdds = undefined

-- === LEVEL 2: Multiple Conditions ===

-- | All numbers from 1 to 100 that are divisible by 3 OR 5
fizzBuzzNumbers :: [Int]
fizzBuzzNumbers = undefined

-- | All numbers from 1 to 50 that are even AND greater than 25
evenAndLarge :: [Int]
evenAndLarge = undefined

-- === LEVEL 3: Working with Tuples ===

{- | Extract all x values from pairs where y > 0
From [(1,-2), (3,4), (5,-6), (7,8)] get [3,7]
-}
positiveYs :: [(Int, Int)] -> [Int]
positiveYs pairs = undefined

-- | Sum each pair: From [(1,2), (3,4)] get [3,7]
pairSums :: [(Int, Int)] -> [Int]
pairSums pairs = undefined

-- === LEVEL 4: Cartesian Products ===

-- | All possible (x,y) coordinates in a 3x3 grid (0 to 2)
grid3x3 :: [(Int, Int)]
grid3x3 = undefined

{- | All possible 2-letter strings from "AB"
Result: ["AA","AB","BA","BB"]
-}
twoLetterWords :: [String]
twoLetterWords = undefined

{- | Playing cards: combine ranks with suits
ranks = "A23", suits = "♠♥", result = ["A♠","A♥","2♠","2♥","3♠","3♥"]
-}
cards :: String -> String -> [String]
cards ranks suits = undefined

-- === LEVEL 5: Dependent Generators ===

-- | All pairs (x,y) where 1 <= x <= y <= 5
orderedPairs :: [(Int, Int)]
orderedPairs = undefined

-- | All triangles (a,b,c) where a+b+c = 12 and a <= b <= c
trianglesSum12 :: [(Int, Int, Int)]
trianglesSum12 = undefined

-- === LEVEL 6: Real-World Patterns ===

-- | Pythagorean triples up to n: a² + b² = c²
pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n = undefined

{- | All non-attacking queen positions on a chessboard
Two queens at (r1,c1) and (r2,c2) attack if:
same row (r1==r2), same col (c1==c2), or same diagonal (|r1-r2| == |c1-c2|)
Return all pairs of positions where queens DON'T attack each other
-}
nonAttackingQueens :: [((Int, Int), (Int, Int))]
nonAttackingQueens = undefined

-- === LEVEL 7: Nested Data ===

-- | Flatten a list of lists (concat)
flatten :: [[a]] -> [a]
flatten lists = undefined

{- | Matrix multiplication position (i,j)
Given two 2x2 matrices as lists of rows
-}
dotProduct :: [[Int]] -> [[Int]] -> Int -> Int -> Int
dotProduct matrix1 matrix2 i j = undefined

-- === CHALLENGE: Refactor these to comprehensions ===

-- | Original ugly code - make it beautiful with a comprehension!
uglyFilter :: [Int] -> [Int]
uglyFilter xs = map (* 3) $ filter (\x -> x > 10) $ filter even xs

-- | This nested mess needs comprehension love
nestedMess :: [(String, Int)] -> [String]
nestedMess pairs =
  concat $
    map
      ( \(name, score) ->
          if score > 80
            then replicate (score `div` 10) name
            else []
      )
      pairs
