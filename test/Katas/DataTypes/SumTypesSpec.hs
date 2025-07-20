module Katas.DataTypes.SumTypesSpec (spec) where

import Katas.DataTypes.SumTypes
import Test.Hspec

spec :: Spec
spec = do
  describe "Direction operations" $ do
    it "turns north clockwise to east" $
      turnClockwise North `shouldBe` East

    it "turns east clockwise to south" $
      turnClockwise East `shouldBe` South

    it "turns south clockwise to west" $
      turnClockwise South `shouldBe` West

    it "turns west clockwise to north" $
      turnClockwise West `shouldBe` North

  describe "Shape operations" $ do
    it "calculates circle area" $
      area (Circle 5) `shouldBe` (pi * 25)

    it "calculates rectangle area" $
      area (Rectangle 4 5) `shouldBe` 20

    it "calculates triangle area" $
      area (Triangle 3 4 5) `shouldBe` 6 -- right triangle
    it "calculates circle perimeter" $
      perimeter (Circle 5) `shouldBe` (2 * pi * 5)

    it "calculates rectangle perimeter" $
      perimeter (Rectangle 4 5) `shouldBe` 18

  describe "Contact formatting" $ do
    it "formats email" $
      formatContact (Email "john@example.com") `shouldBe` "Email: john@example.com"

    it "formats phone" $
      formatContact (Phone "+1-555-0123") `shouldBe` "Phone: +1-555-0123"

    it "formats address" $
      formatContact (Address "123 Main St" "Springfield" "12345")
        `shouldBe` "Address: 123 Main St, Springfield 12345"
