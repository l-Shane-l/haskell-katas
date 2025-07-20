module Katas.TypeClasses.NewtypeInstancesSpec (spec) where

import Katas.TypeClasses.NewtypeInstances
import Test.Hspec

spec :: Spec
spec = do
  describe "Email" $ do
    it "creates valid email" $
      mkEmail "user@example.com" `shouldBe` Just (Email "user@example.com")

    it "rejects invalid email - no @" $
      mkEmail "userexample.com" `shouldBe` Nothing

    it "rejects invalid email - no domain" $
      mkEmail "user@example" `shouldBe` Nothing

    it "extracts email string" $
      case mkEmail "test@test.com" of
        Just e -> getEmail e `shouldBe` "test@test.com"
        Nothing -> error "Should not fail"

  describe "Age" $ do
    it "creates valid age" $
      mkAge 25 `shouldBe` Just (Age 25)

    it "rejects negative age" $
      mkAge (-5) `shouldBe` Nothing

    it "rejects age over 150" $
      mkAge 200 `shouldBe` Nothing

    it "allows age 0" $
      mkAge 0 `shouldBe` Just (Age 0)

    it "allows age 150" $
      mkAge 150 `shouldBe` Just (Age 150)

  describe "USD" $ do
    it "adds money" $
      dollars 5 + dollars 3 `shouldBe` dollars 8

    it "subtracts money" $
      dollars 10 - dollars 3 `shouldBe` dollars 7

    it "multiplies money (as dollar amounts)" $
      dollars 5 * dollars 3 `shouldBe` dollars 15

    it "creates from integer (as dollars)" $
      42 ::
      USD `shouldBe` dollars 42

    it "shows as dollars" $ do
      showDollars (dollars 5) `shouldBe` "$5.00"
      showDollars (USD 550) `shouldBe` "$5.50"
      showDollars (USD 42) `shouldBe` "$0.42"

