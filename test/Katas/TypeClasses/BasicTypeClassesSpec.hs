module Katas.TypeClasses.BasicTypeClassesSpec (spec) where

import Katas.TypeClasses.BasicTypeClasses
import Test.Hspec

spec :: Spec
spec = do
  describe "Reversible" $ do
    it "reverses strings" $ do
      rev "hello" `shouldBe` "olleh"
      rev "" `shouldBe` ""

    it "reverses lists" $ do
      rev [1, 2, 3, 4, 5] `shouldBe` [5, 4, 3, 2, 1]
      rev ([] :: [Int]) `shouldBe` []

  describe "Default" $ do
    it "provides default for Int" $
      def `shouldBe` (0 :: Int)

    it "provides default for String" $
      def `shouldBe` ""

    it "provides default for Bool" $
      def `shouldBe` False

    it "provides default for lists" $
      def `shouldBe` ([] :: [Int])

    it "provides default for Maybe" $
      def `shouldBe` (Nothing :: Maybe String)

  describe "fillDefaults" $ do
    it "fills Nothing with defaults" $
      fillDefaults [Just 1, Nothing, Just 3, Nothing] `shouldBe` [1, 0, 3, 0]

    it "works with strings" $
      fillDefaults [Just "hello", Nothing, Just "world"] `shouldBe` ["hello", "", "world"]

    it "works with empty list" $
      fillDefaults ([] :: [Maybe Int]) `shouldBe` []

