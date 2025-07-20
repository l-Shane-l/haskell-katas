module Katas.Layer2_Workhorse.LazinessSpec (spec) where

import Control.Exception (evaluate)
import Katas.Layer2_Workhorse.Laziness
import Test.Hspec

spec :: Spec
spec = do
  describe "Laziness and Evaluation" $ do
    describe "Part 1: Space Leaks" $ do
      it "fixes sumSquares space leak" $ do
        -- The good version shouldn't blow up on large lists
        sumSquaresGood [1 .. 1000000] `shouldBe` sumSquaresBad [1 .. 10000]

      it "calculates sum and product in one pass" $ do
        sumAndProductGood [1 .. 5] `shouldBe` (15.0, 120.0)
        -- Should handle larger lists efficiently
        let (s, p) = sumAndProductGood [1 .. 100]
        s `shouldBe` 5050.0

    describe "Part 2: Infinite Lists" $ do
      it "generates powers of two" $ do
        take 5 powersOfTwo `shouldBe` [1, 2, 4, 8, 16]
        powersOfTwo !! 10 `shouldBe` 1024

      it "finds first power greater than n" $ do
        firstPowerGreaterThan 10 `shouldBe` 16
        firstPowerGreaterThan 100 `shouldBe` 128
        firstPowerGreaterThan 1000 `shouldBe` 1024

      it "generates Fibonacci numbers" $ do
        take 10 fibs `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
        fibs !! 20 `shouldBe` 6765

    describe "Part 3: Lazy vs Strict Trade-offs" $ do
      it "takes n elements satisfying predicate" $ do
        takeWhileN 3 even [2, 4, 5, 6, 8, 10] `shouldBe` [2, 4, 6]
        takeWhileN 2 (> 5) [1 ..] `shouldBe` [6, 7]

      it "finds nth prime" $ do
        nthPrime 1 `shouldBe` 2
        nthPrime 10 `shouldBe` 29
        nthPrime 100 `shouldBe` 541

      it "processes until STOP lazily" $ do
        let input = ["line1", "line2", "STOP", "should not appear"]
        processUntilStop input `shouldBe` ["line1", "line2"]

    describe "Part 4: Understanding Evaluation" $ do
      it "example1 evaluates without error" $ do
        example1 `shouldBe` 3

      it "dangerousTake works for valid indices" $ do
        dangerousTake 2 [1, 2, 3, 4] `shouldBe` [1, 2]

      it "dangerousTake prevents access beyond n" $ do
        let list = dangerousTake 2 [1, 2, 3, 4]
        evaluate (list !! 2) `shouldThrow` anyException

    describe "Part 5: Practical Laziness" $ do
      it "generates Pythagorean triples" $ do
        take 5 pythagoreanTriples
          `shouldBe` [(3, 4, 5), (6, 8, 10), (5, 12, 13), (9, 12, 15), (8, 15, 17)]

      it "merges sorted lists" $ do
        take 10 (mergeSorted [1, 3 ..] [2, 4 ..]) `shouldBe` [1 .. 10]
        take 5 (mergeSorted [1, 1, 2, 3] [1, 2, 2, 4]) `shouldBe` [1, 1, 1, 2, 2]

      it "generates Hamming sequence" $ do
        take 20 hamming
          `shouldBe` [1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36]
