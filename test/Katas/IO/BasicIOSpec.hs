module Katas.IO.BasicIOSpec (spec) where

import Katas.IO.BasicIO
import System.IO.Silently
import Test.Hspec

spec :: Spec
spec = do
  describe "Basic IO Operations" $ do
    describe "echo" $ do
      it "echoes user input with prefix" $ do
        output <- capture_ $ withInput "Hello\n" echo
        output `shouldBe` "You said: Hello\n"

    describe "squareInput" $ do
      it "squares valid input" $ do
        output <- capture_ $ withInput "5\n" squareInput
        output `shouldBe` "25\n"

      it "handles invalid input" $ do
        output <- capture_ $ withInput "abc\n" squareInput
        output `shouldBe` "Invalid number\n"

    describe "greetUser" $ do
      it "greets with full name" $ do
        output <- capture_ $ withInput "John\nDoe\n" greetUser
        output `shouldBe` "First name: Last name: Hello, John Doe!\n"

    describe "readInt" $ do
      it "parses valid integer" $ do
        result <- withInput "42\n" readInt
        result `shouldBe` Just 42

      it "returns Nothing for invalid input" $ do
        result <- withInput "hello\n" readInt
        result `shouldBe` Nothing

-- Helper to provide input to IO actions
withInput :: String -> IO a -> IO a
withInput input action = do
  let stubInput = const (return input)
  capture_ $ hWithIn stubInput action

-- This is a simplified version - real implementation would need
-- to properly mock stdin
hWithIn :: (String -> IO String) -> IO a -> IO a
hWithIn = undefined -- Provided by test framework

