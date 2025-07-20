module Katas.Layer2_Workhorse.ListPatternsSpec (spec) where

import Data.List (sort)
import Katas.Layer2_Workhorse.ListPatterns
import Test.Hspec

spec :: Spec
spec = do
  describe "List Pattern Exercises" $ do
    describe "countElements" $ do
      it "counts empty list" $
        countElements ([] :: [Int]) `shouldBe` 0
      it "counts non-empty list" $
        countElements [1, 2, 3, 4, 5] `shouldBe` 5

    describe "lastElement" $ do
      it "returns Nothing for empty list" $
        lastElement ([] :: [Int]) `shouldBe` Nothing
      it "returns last element" $
        lastElement [1, 2, 3] `shouldBe` Just 3
      it "handles single element" $
        lastElement [42] `shouldBe` Just 42

    describe "everyNth" $ do
      it "gets every 2nd element" $
        everyNth 2 [1, 2, 3, 4, 5, 6] `shouldBe` [2, 4, 6]
      it "gets every 3rd element" $
        everyNth 3 [1 .. 9] `shouldBe` [3, 6, 9]
      it "handles n larger than list" $
        everyNth 10 [1, 2, 3] `shouldBe` []

    describe "runLengthEncode" $ do
      it "encodes empty list" $
        runLengthEncode ([] :: [Char]) `shouldBe` []
      it "encodes repeated characters" $
        runLengthEncode "aaabbcaaa" `shouldBe` [(3, 'a'), (2, 'b'), (1, 'c'), (3, 'a')]
      it "handles single characters" $
        runLengthEncode "abcd" `shouldBe` [(1, 'a'), (1, 'b'), (1, 'c'), (1, 'd')]

    describe "positions" $ do
      it "finds positions in string" $
        positions 'a' "abacad" `shouldBe` [0, 2, 4]
      it "returns empty for not found" $
        positions 'x' "hello" `shouldBe` []
      it "finds all occurrences" $
        positions 1 [1, 2, 1, 3, 1] `shouldBe` [0, 2, 4]

    describe "chunksOf" $ do
      it "splits into chunks" $
        chunksOf 3 [1 .. 10] `shouldBe` [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10]]
      it "handles exact multiple" $
        chunksOf 2 [1, 2, 3, 4] `shouldBe` [[1, 2], [3, 4]]
      it "handles empty list" $
        chunksOf 3 ([] :: [Int]) `shouldBe` []

    describe "applyAll" $ do
      it "applies multiple functions" $
        applyAll [(* 2), (+ 10), (`div` 3)] 9 `shouldBe` [18, 19, 3]
      it "handles empty function list" $
        applyAll [] 5 `shouldBe` ([] :: [Int])

    describe "frequency" $ do
      it "counts character frequency" $
        sort (frequency "hello") `shouldBe` sort [('h', 1), ('e', 1), ('l', 2), ('o', 1)]
      it "handles empty list" $
        frequency ([] :: [Char]) `shouldBe` []

    describe "longestRun" $ do
      it "finds longest consecutive sequence" $
        longestRun [1, 1, 2, 2, 2, 3, 3, 3, 3, 2, 2] `shouldBe` 4
      it "handles no repeats" $
        longestRun [1, 2, 3, 4, 5] `shouldBe` 1
      it "handles empty list" $
        longestRun ([] :: [Int]) `shouldBe` 0

    -- UPDATED TEST SUITE
    describe "interleave" $ do
      it "interleaves equal length lists" $
        interleave [1, 2, 3] [4, 5, 6] `shouldBe` [1, 4, 2, 5, 3, 6]
      it "handles different lengths" $
        interleave [1, 2, 3] [4, 5, 6, 7, 8] `shouldBe` [1, 4, 2, 5, 3, 6]
      it "handles empty lists" $
        interleave [] [1, 2, 3] `shouldBe` [1, 2, 3]
