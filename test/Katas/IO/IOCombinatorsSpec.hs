module Katas.IO.IOCombinatorsSpec (spec) where

import Data.IORef
import Katas.IO.IOCombinators
import System.IO.Silently
import Test.Hspec

spec :: Spec
spec = do
  describe "IO Combinators" $ do
    describe "when'" $ do
      it "executes action when True" $ do
        output <- capture_ $ when' True (putStr "executed")
        output `shouldBe` "executed"

      it "does nothing when False" $ do
        output <- capture_ $ when' False (putStr "not executed")
        output `shouldBe` ""

    describe "sequence'" $ do
      it "sequences IO actions and collects results" $ do
        ref <- newIORef []
        let actions = [modifyIORef ref (1 :), modifyIORef ref (2 :), modifyIORef ref (3 :)]
        _ <- sequence' actions
        result <- readIORef ref
        reverse result `shouldBe` [1, 2, 3]

      it "maintains order" $ do
        output <- capture_ $ do
          sequence' [putStr "A", putStr "B", putStr "C"]
        output `shouldBe` "ABC"

    describe "sequence_'" $ do
      it "sequences actions ignoring results" $ do
        output <- capture_ $ sequence_' [putStr "X", putStr "Y", putStr "Z"]
        output `shouldBe` "XYZ"

    describe "mapM'" $ do
      it "maps IO function over list" $ do
        output <- capture_ $ do
          mapM' putStr ["one", "two", "three"]
        output `shouldBe` "onetwothree"

      it "collects results" $ do
        results <- mapM' (\x -> return (x * 2)) [1, 2, 3]
        results `shouldBe` [2, 4, 6]

    describe "andThen" $ do
      it "chains IO actions" $ do
        output <- capture_ $ do
          (putStr "first") `andThen` (\_ -> putStr "second")
        output `shouldBe` "firstsecond"

      it "passes value through" $ do
        result <- (return 5) `andThen` (\x -> return (x * 2))
        result `shouldBe` 10

    describe "addLineNumbers" $ do
      it "adds line numbers to multiline string" $ do
        result <- addLineNumbers (return "Hello\nWorld\nTest")
        result `shouldBe` "1: Hello\n2: World\n3: Test"

      it "handles single line" $ do
        result <- addLineNumbers (return "Single line")
        result `shouldBe` "1: Single line"

      it "handles empty string" $ do
        result <- addLineNumbers (return "")
        result `shouldBe` ""
