module Katas.Layer2_Workhorse.Chapter2ExercisesSpec (spec) where

import qualified Data.List as List
import Katas.Layer2_Workhorse.Chapter2Exercises
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter 2 Exercises" $ do
    describe "Exercise 1: Reversing with Folds" $ do
      it "reverses with foldl" $ do
        reverseWithFoldl [1, 2, 3, 4, 5] `shouldBe` [5, 4, 3, 2, 1]
        reverseWithFoldl "hello" `shouldBe` "olleh"
        reverseWithFoldl ([] :: [Int]) `shouldBe` []

      it "reverses with foldr" $ do
        reverseWithFoldr [1, 2, 3, 4, 5] `shouldBe` [5, 4, 3, 2, 1]
        reverseWithFoldr "hello" `shouldBe` "olleh"
        reverseWithFoldr ([] :: [Int]) `shouldBe` []

      it "has analysis" $
        length reverseAnalysis > 20 `shouldBe` True

    describe "Exercise 2: Zipping Lists" $ do
      let testZipWith impl = do
            impl (+) [1, 2, 3] [4, 5, 6] `shouldBe` [5, 7, 9]
            impl (*) [1, 2] [3, 4, 5] `shouldBe` [3, 8]
            impl (,) [1, 2] ['a', 'b'] `shouldBe` [(1, 'a'), (2, 'b')]
            impl (+) [] [1, 2, 3] `shouldBe` ([] :: [Int])

      it "zipWith with recursion" $
        testZipWith zipWithRecursive

      it "zipWith with comprehension" $
        testZipWith zipWithComprehension

      it "zipWith with fold (if possible)" $
        -- This might not be possible!
        pending

      it "has analysis" $
        length zipWithFoldAnalysis > 20 `shouldBe` True

    describe "Exercise 3: concatMap" $ do
      it "concat works" $ do
        concat [[1, 2], [3, 4], [5]] `shouldBe` [1, 2, 3, 4, 5]
        concat [[], [1], []] `shouldBe` [1]
        concat ([] :: [[Int]]) `shouldBe` []

      let testConcatMap impl = do
            impl (\x -> [x, x]) [1, 2, 3] `shouldBe` [1, 1, 2, 2, 3, 3]
            impl (\x -> if even x then [x] else []) [1, 2, 3, 4] `shouldBe` [2, 4]
            impl (\x -> [x .. x + 2]) [1, 3, 5] `shouldBe` [1, 2, 3, 3, 4, 5, 5, 6, 7]

      it "concatMap with foldl" $
        testConcatMap concatMapFoldl

      it "concatMap with foldr" $
        testConcatMap concatMapFoldr

      it "has analysis" $
        length concatMapAnalysis > 20 `shouldBe` True

    describe "Exercise 4: Maps and Folds" $ do
      it "shows when they're the same" $
        sameBehaviorExample `shouldBe` True

      it "shows when they differ" $
        differentBehaviorExample `shouldBe` True

      it "has analysis" $
        length mapFoldAnalysis > 20 `shouldBe` True

    describe "Exercise 5: Infinite Lists" $ do
      it "has infinite list analysis" $
        length foldlInfiniteAnalysis > 50 `shouldBe` True

      it "finds first greater" $ do
        findFirstGreater 100 [1, 50, 75, 150, 200] `shouldBe` Just 150
        findFirstGreater 10 [1 ..] `shouldBe` Just 11
        findFirstGreater 100 [1, 2, 3] `shouldBe` Nothing

      it "demonstrates early termination" $ do
        earlyTermination [1 .. 2000] `shouldBe` True
        earlyTermination [1 .. 999] `shouldBe` False
        -- Should not evaluate the whole infinite list!
        earlyTermination [1 ..] `shouldBe` True

    describe "Bonus: Fibonacci Variations" $ do
      it "generates fibonacci numbers" $ do
        take 10 fibs `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
        fibs !! 20 `shouldBe` 6765

      it "generates fibonacci from any start" $ do
        take 5 (fibsFrom 2 3) `shouldBe` [2, 3, 5, 8, 13]
        take 5 (fibsFrom 10 10) `shouldBe` [10, 10, 20, 30, 50]

      it "approximates golden ratio" $ do
        let ratios = take 10 $ drop 10 goldenRatios
        all (\r -> abs (r - 1.618033988749895) < 0.001) ratios `shouldBe` True

    describe "Challenge: Stream Processing" $ do
      it "generates Hamming numbers" $
        take 20 hamming
          `shouldBe` [1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36]
      it "merges sorted streams" $ do
        take 10 (mergeSorted [1, 3 ..] [2, 4 ..]) `shouldBe` [1 .. 10]
        take 5 (mergeSorted [1, 1, 2, 3] [1, 2, 2, 4]) `shouldBe` [1, 1, 1, 2, 2]

