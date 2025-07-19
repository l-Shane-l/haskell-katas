module Katas.Layer1_Fundamentals.ExercisesSpec (spec) where

import Katas.Layer1_Fundamentals.Exercises
import Test.Hspec

spec :: Spec
spec = do
  describe "FizzBuzz" $ do
    it "handles single number" $
      fizzBuzz 1 `shouldBe` "1 "

    it "handles fizz" $
      fizzBuzz 3 `shouldBe` "1 2 fizz "

    it "handles buzz" $
      fizzBuzz 5 `shouldBe` "1 2 fizz 4 buzz "

    it "handles fizzbuzz at 15" $
      fizzBuzz 15 `shouldBe` "1 2 fizz 4 buzz fizz 7 8 fizz buzz 11 fizz 13 14 fizzbuzz "

    describe "fizzBuzzFor helper" $ do
      it "returns number as string for non-multiples" $
        fizzBuzzFor 1 `shouldBe` "1"

      it "returns fizz for multiples of 3 only" $
        fizzBuzzFor 3 `shouldBe` "fizz"

      it "returns buzz for multiples of 5 only" $
        fizzBuzzFor 5 `shouldBe` "buzz"

      it "returns fizzbuzz for multiples of 15" $
        fizzBuzzFor 15 `shouldBe` "fizzbuzz"

  describe "Factorial" $ do
    it "factorial of 0 is 1" $
      factorial 0 `shouldBe` 1

    it "factorial of 1 is 1" $
      factorial 1 `shouldBe` 1

    it "factorial of 3 is 6" $
      factorial 3 `shouldBe` 6

    it "factorial of 5 is 120" $
      factorial 5 `shouldBe` 120

    it "factorial of 10 is 3628800" $
      factorial 10 `shouldBe` 3628800

    it "handles large numbers" $
      factorial 25 `shouldBe` 15511210043330985984000000

  describe "Fibonacci" $ do
    it "fibonacci 0 is 0" $
      fibonacci 0 `shouldBe` 0

    it "fibonacci 1 is 1" $
      fibonacci 1 `shouldBe` 1

    it "fibonacci 5 is 5" $
      fibonacci 5 `shouldBe` 5

    it "fibonacci 10 is 55" $
      fibonacci 10 `shouldBe` 55

    it "fibonacci 25 is 75025" $
      fibonacci 25 `shouldBe` 75025

  describe "Manual Currying" $ do
    let uncurriedAdd (a, b) = a + b
        curriedAdd = myCurry uncurriedAdd

    it "myCurry creates a curried function" $ do
      curriedAdd 1 2 `shouldBe` 3
      curriedAdd 5 10 `shouldBe` 15

    it "myCurry allows partial application" $ do
      let addOne = curriedAdd 1
      addOne 5 `shouldBe` 6
      addOne 10 `shouldBe` 11

    it "myUncurry creates an uncurried function" $ do
      let uncurriedPlus = myUncurry (+)
      uncurriedPlus (1, 2) `shouldBe` 3
      uncurriedPlus (5, 10) `shouldBe` 15

  describe "Additional Recursion Practice" $ do
    describe "sumTo" $ do
      it "sums 1 to 1" $
        sumTo 1 `shouldBe` 1

      it "sums 1 to 5" $
        sumTo 5 `shouldBe` 15

      it "sums 1 to 10" $
        sumTo 10 `shouldBe` 55

    describe "countDown" $ do
      it "counts down from 1" $
        countDown 1 `shouldBe` "1 Done!"

      it "counts down from 5" $
        countDown 5 `shouldBe` "5 4 3 2 1 Done!"

      it "handles 0" $
        countDown 0 `shouldBe` "Done!"

    describe "doubleUntil" $ do
      it "doubles until exceeding 10" $
        doubleUntil 10 `shouldBe` 16

      it "doubles until exceeding 100" $
        doubleUntil 100 `shouldBe` 128

      it "returns 1 when target is 1" $
        doubleUntil 1 `shouldBe` 1

