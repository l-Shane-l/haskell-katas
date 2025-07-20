module Katas.TypeClasses.HigherKindedSpec (spec) where

import Katas.TypeClasses.HigherKinded
import Test.Hspec

spec :: Spec
spec = do
  describe "Compress" $ do
    describe "for lists" $ do
      it "removes consecutive duplicates" $
        compress [1, 1, 2, 2, 2, 3, 1, 1] `shouldBe` [1, 2, 3, 1]

      it "handles empty list" $
        compress ([] :: [Int]) `shouldBe` []

      it "handles single element" $
        compress [1] `shouldBe` [1]

      it "handles no duplicates" $
        compress [1, 2, 3, 4] `shouldBe` [1, 2, 3, 4]

    describe "for Maybe" $ do
      it "keeps Nothing as Nothing" $
        compress (Nothing :: Maybe Int) `shouldBe` Nothing

      it "keeps Just as Just" $
        compress (Just 5) `shouldBe` Just 5

  describe "Filterable" $ do
    describe "for lists" $ do
      it "filters even numbers" $
        filterF even [1, 2, 3, 4, 5, 6] `shouldBe` [2, 4, 6]

      it "filters empty list" $
        filterF even ([] :: [Int]) `shouldBe` []

    describe "for Maybe" $ do
      it "keeps Just when predicate true" $
        filterF even (Just 4) `shouldBe` Just 4

      it "removes Just when predicate false" $
        filterF even (Just 3) `shouldBe` Nothing

      it "keeps Nothing as Nothing" $
        filterF even (Nothing :: Maybe Int) `shouldBe` Nothing

