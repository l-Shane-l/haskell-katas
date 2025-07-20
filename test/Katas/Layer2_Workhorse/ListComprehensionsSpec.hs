module Katas.Layer2_Workhorse.ListComprehensionsSpec (spec) where

import Data.List (sort)
import Katas.Layer2_Workhorse.ListComprehensions
import Test.Hspec

spec :: Spec
spec = do
  describe "List Comprehension Muscle Memory" $ do
    describe "Level 1: Basic Transformations" $ do
      it "doubles all numbers" $
        doubles `shouldBe` [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]

      it "keeps only evens" $
        evens `shouldBe` [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]

      it "squares odd numbers" $
        squaredOdds `shouldBe` [1, 9, 25, 49, 81]

    describe "Level 2: Multiple Conditions" $ do
      it "finds fizzbuzz numbers" $
        take 10 fizzBuzzNumbers `shouldBe` [3, 5, 6, 9, 10, 12, 15, 18, 20, 21]

      it "finds even and large" $
        evenAndLarge `shouldBe` [26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50]

    describe "Level 3: Working with Tuples" $ do
      it "extracts x where y > 0" $
        positiveYs [(1, -2), (3, 4), (5, -6), (7, 8)] `shouldBe` [3, 7]

      it "sums pairs" $
        pairSums [(1, 2), (3, 4), (5, 6)] `shouldBe` [3, 7, 11]

    describe "Level 4: Cartesian Products" $ do
      it "creates 3x3 grid" $
        grid3x3 `shouldBe` [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)]

      it "creates two letter words" $
        twoLetterWords `shouldBe` ["AA", "AB", "BA", "BB"]

      it "creates playing cards" $
        cards "A23" "♠♥" `shouldBe` ["A♠", "A♥", "2♠", "2♥", "3♠", "3♥"]

    describe "Level 5: Dependent Generators" $ do
      it "creates ordered pairs" $
        orderedPairs
          `shouldBe` [ (1, 1)
                     , (1, 2)
                     , (1, 3)
                     , (1, 4)
                     , (1, 5)
                     , (2, 2)
                     , (2, 3)
                     , (2, 4)
                     , (2, 5)
                     , (3, 3)
                     , (3, 4)
                     , (3, 5)
                     , (4, 4)
                     , (4, 5)
                     , (5, 5)
                     ]

      it "finds triangles with sum 12" $
        trianglesSum12 `shouldBe` [(1, 5, 6), (2, 4, 6), (2, 5, 5), (3, 3, 6), (3, 4, 5), (4, 4, 4)]

    describe "Level 6: Real-World Patterns" $ do
      it "finds pythagorean triples" $
        pythagoreanTriples 15 `shouldBe` [(3, 4, 5), (5, 12, 13), (6, 8, 10), (9, 12, 15)]

      it "finds non-attacking queen pairs" $ do
        let queens = take 10 nonAttackingQueens
        -- Check first few valid pairs
        queens `shouldContain` [((0, 0), (1, 2))]
        queens `shouldContain` [((0, 0), (2, 1))]
        -- Verify no queens on same row/col/diagonal
        all
          ( \((r1, c1), (r2, c2)) ->
              r1 /= r2 && c1 /= c2 && abs (r1 - r2) /= abs (c1 - c2)
          )
          queens
          `shouldBe` True

    describe "Level 7: Nested Data" $ do
      it "flattens lists" $
        flatten [[1, 2], [3], [4, 5, 6]] `shouldBe` [1, 2, 3, 4, 5, 6]

      it "computes matrix dot product" $ do
        let m1 = [[1, 2], [3, 4]]
            m2 = [[5, 6], [7, 8]]
        dotProduct m1 m2 0 0 `shouldBe` 19 -- 1*5 + 2*7
        dotProduct m1 m2 0 1 `shouldBe` 22 -- 1*6 + 2*8
        dotProduct m1 m2 1 0 `shouldBe` 43 -- 3*5 + 4*7
        dotProduct m1 m2 1 1 `shouldBe` 50 -- 3*6 + 4*8
    describe "Challenge: Refactoring" $ do
      it "refactors ugly filter" $
        uglyFilter [1 .. 20] `shouldBe` [36, 42, 48, 54, 60]

      it "refactors nested mess" $
        sort (nestedMess [("Alice", 95), ("Bob", 70), ("Carol", 85)])
          `shouldBe` sort
            [ "Alice"
            , "Alice"
            , "Alice"
            , "Alice"
            , "Alice"
            , "Alice"
            , "Alice"
            , "Alice"
            , "Alice"
            , "Carol"
            , "Carol"
            , "Carol"
            , "Carol"
            , "Carol"
            , "Carol"
            , "Carol"
            , "Carol"
            ]
