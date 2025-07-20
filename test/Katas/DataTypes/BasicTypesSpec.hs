module Katas.DataTypes.BasicTypesSpec (spec) where

import Katas.DataTypes.BasicTypes
import Test.Hspec

spec :: Spec
spec = do
  describe "Point operations" $ do
    it "creates a point" $
      mkPoint 3 4 `shouldBe` Point 3 4

    it "calculates distance from origin" $
      distanceFromOrigin (Point 3 4) `shouldBe` 5.0

    it "calculates distance for point at origin" $
      distanceFromOrigin (Point 0 0) `shouldBe` 0.0

  describe "Person operations" $ do
    it "creates a valid person" $
      mkPerson "John" "Doe" 25 `shouldBe` Just (Person "John" "Doe" 25)

    it "rejects negative age" $
      mkPerson "John" "Doe" (-5) `shouldBe` Nothing

    it "rejects age over 150" $
      mkPerson "John" "Doe" 200 `shouldBe` Nothing

    it "increments age on birthday" $
      birthday (Person "Jane" "Doe" 30) `shouldBe` Person "Jane" "Doe" 31

    it "formats full name" $
      fullName (Person "John" "Smith" 25) `shouldBe` "John Smith"
