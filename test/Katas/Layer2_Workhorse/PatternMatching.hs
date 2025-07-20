module Katas.Layer2_Workhorse.PatternMatchingSpec (spec) where

import Katas.Layer2_Workhorse.PatternMatching
import Test.Hspec

spec :: Spec
spec = do
  describe "Pattern Matching Mastery" $ do
    describe "Level 1: Basic Patterns" $ do
      it "describes numbers" $ do
        describeNumber 0 `shouldBe` "zero"
        describeNumber 1 `shouldBe` "one"
        describeNumber (-5) `shouldBe` "negative"
        describeNumber 42 `shouldBe` "positive"

      it "safely gets head" $ do
        safeHead [1, 2, 3] `shouldBe` Just 1
        safeHead ([] :: [Int]) `shouldBe` Nothing

      it "safely gets tail" $ do
        safeTail [1, 2, 3] `shouldBe` [2, 3]
        safeTail ([] :: [Int]) `shouldBe` []

      it "greets people" $ do
        greet "Alice" `shouldBe` "Hey Alice, how's Bob?"
        greet "Bob" `shouldBe` "Yo Bob, how's Alice?"
        greet "Charlie" `shouldBe` "Hello, Charlie!"

    describe "Level 2: Tuple Patterns" $ do
      it "extracts age from different formats" $ do
        getAge (Left ("Alice", 30)) `shouldBe` 30
        getAge (Right ("Bob", 25, "NYC")) `shouldBe` 25

      it "swaps if sum > 10" $ do
        swapIfBig (3, 8) `shouldBe` (8, 3)
        swapIfBig (2, 3) `shouldBe` (2, 3)
        swapIfBig (10, 1) `shouldBe` (1, 10)

      it "describes points" $ do
        describePoint (0, 0) `shouldBe` "at origin"
        describePoint (0, 5) `shouldBe` "on y-axis at 5"
        describePoint (3, 0) `shouldBe` "on x-axis at 3"
        describePoint (3, 4) `shouldBe` "at point (3,4)"

    describe "Level 3: List Patterns" $ do
      it "identifies lists of two" $ do
        isListOfTwo [1, 2] `shouldBe` True
        isListOfTwo [1] `shouldBe` False
        isListOfTwo [1, 2, 3] `shouldBe` False
        isListOfTwo ([] :: [Int]) `shouldBe` False

      it "gets second element" $ do
        secondElement [1, 2, 3] `shouldBe` Just 2
        secondElement [1] `shouldBe` Nothing
        secondElement ([] :: [Int]) `shouldBe` Nothing

      it "sums first three" $ do
        sumFirstThree [1, 2, 3, 4, 5] `shouldBe` 6
        sumFirstThree [10, 20] `shouldBe` 30
        sumFirstThree [5] `shouldBe` 5
        sumFirstThree ([] :: [Int]) `shouldBe` 0

      it "describes list length" $ do
        describeList ([] :: [Int]) `shouldBe` "empty"
        describeList [1] `shouldBe` "singleton"
        describeList [1, 2] `shouldBe` "pair"
        describeList [1, 2, 3] `shouldBe` "triple"
        describeList [1, 2, 3, 4] `shouldBe` "list of many"

    describe "Level 4: Nested Patterns" $ do
      it "gets first of first" $ do
        firstOfFirst [[1, 2], [3, 4]] `shouldBe` Just 1
        firstOfFirst [[], [1, 2]] `shouldBe` Nothing
        firstOfFirst ([] :: [[Int]]) `shouldBe` Nothing

      it "checks if starts with pair" $ do
        startsWith 1 2 [1, 2, 3, 4] `shouldBe` True
        startsWith 1 2 [1, 3, 2, 4] `shouldBe` False
        startsWith 1 2 [1] `shouldBe` False

      it "extracts nested data" $ do
        extractNested (Just (Left (42, "test"))) `shouldBe` 42
        extractNested (Just (Right "test")) `shouldBe` 0
        extractNested Nothing `shouldBe` (-1)

    describe "Level 5: As-Patterns" $ do
      it "describes list and head" $ do
        describeListAndHead [1, 2, 3] `shouldBe` "The list [1,2,3] starts with 1"
        describeListAndHead ([] :: [Int]) `shouldBe` "The list is empty"

      it "finds first duplicate" $ do
        firstDuplicate [(1, 2), (1, 3)] `shouldBe` Just (1, 2)
        firstDuplicate [(1, 2), (2, 3)] `shouldBe` Nothing
        firstDuplicate [(1, 2), (2, 3), (1, 4)] `shouldBe` Just (1, 2)

    describe "Level 6: Guards with Patterns" $ do
      it "classifies int lists" $ do
        classifyIntList [] `shouldBe` "empty"
        classifyIntList [-5] `shouldBe` "single negative"
        classifyIntList [5] `shouldBe` "single positive"
        classifyIntList [1, 2, 3] `shouldBe` "all positive"
        classifyIntList [-1, -2, -3] `shouldBe` "all negative"
        classifyIntList [1, -2, 3] `shouldBe` "mixed"

      it "validates input" $ do
        validateInput Nothing `shouldBe` "missing"
        validateInput (Just (-5)) `shouldBe` "negative not allowed"
        validateInput (Just 0) `shouldBe` "zero"
        validateInput (Just 150) `shouldBe` "too large"
        validateInput (Just 50) `shouldBe` "valid: 50"

    describe "Level 7: Case Expressions" $ do
      it "parses commands" $ do
        parseCommand "quit" `shouldBe` "Goodbye!"
        parseCommand "help" `shouldBe` "Available commands: quit, help, add"
        parseCommand "add 5 3" `shouldBe` "Result: 8"
        parseCommand "random" `shouldBe` "Unknown command: random"

      it "processes nested Maybe" $ do
        processNested Nothing `shouldBe` 0
        processNested (Just Nothing) `shouldBe` (-1)
        processNested (Just (Just 42)) `shouldBe` 42

    describe "Level 8: Complex Patterns" $ do
      it "run-length decodes" $ do
        runLengthDecode [(3, 'a'), (2, 'b'), (1, 'c')] `shouldBe` "aaabbc"
        runLengthDecode [] `shouldBe` ""
        runLengthDecode [(0, 'x'), (2, 'y')] `shouldBe` "yy"

      it "finds first Just" $ do
        firstJust [Nothing, Nothing, Just 3, Just 4] `shouldBe` Just 3
        firstJust [Nothing, Nothing] `shouldBe` (Nothing :: Maybe Int)
        firstJust [] `shouldBe` (Nothing :: Maybe Int)

      it "zips until zero" $ do
        zipUntilZero [1, 2, 0, 4] [5, 6, 7, 8] `shouldBe` [(1, 5), (2, 6)]
        zipUntilZero [1, 2, 3] [4, 0, 6] `shouldBe` [(1, 4)]
        zipUntilZero [0, 1, 2] [3, 4, 5] `shouldBe` []

      it "compares two numbers" $ do
        compareTwo 0 5 `shouldBe` "first is zero"
        compareTwo 5 0 `shouldBe` "second is zero"
        compareTwo 3 3 `shouldBe` "equal"
        compareTwo 5 3 `shouldBe` "first is larger"
        compareTwo 3 5 `shouldBe` "second is larger"

    describe "Challenge: Safe Functions" $ do
      it "safely divides" $ do
        safeDiv 10 2 `shouldBe` Just 5
        safeDiv 10 0 `shouldBe` Nothing

      it "safely gets nth element" $ do
        safeNth 0 [1, 2, 3] `shouldBe` Just 1
        safeNth 2 [1, 2, 3] `shouldBe` Just 3
        safeNth 5 [1, 2, 3] `shouldBe` Nothing
        safeNth 0 [] `shouldBe` (Nothing :: Maybe Int)

      it "parses coordinates" $ do
        parseCoordinate "(3,4)" `shouldBe` Just (3, 4)
        parseCoordinate "(0,0)" `shouldBe` Just (0, 0)
        parseCoordinate "(-5,10)" `shouldBe` Just (-5, 10)
        parseCoordinate "invalid" `shouldBe` Nothing
        parseCoordinate "(a,b)" `shouldBe` Nothing
