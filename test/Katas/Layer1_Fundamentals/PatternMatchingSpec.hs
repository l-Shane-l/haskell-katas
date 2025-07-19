module Katas.Layer1_Fundamentals.PatternMatchingSpec (spec) where

import Katas.Layer1_Fundamentals.PatternMatching
import Test.Hspec

spec :: Spec
spec = do
  describe "Pattern Matching Drills" $ do
    it "describes an empty list" $
      describeList ([] :: [Int]) `shouldBe` "Empty"
    it "describes a non-empty list" $
      describeList [1, 2, 3] `shouldBe` "Non-empty"

    it "gets the first element of a tuple" $
      getFirst (10, "hello") `shouldBe` 10

    it "unwraps a Just value" $
      unwrapMaybe (Just 5) 0 `shouldBe` 5
    it "unwraps a Nothing value to the default" $
      unwrapMaybe Nothing 0 `shouldBe` 0

    it "handles a Left value" $
      -- We specify that the 'Right' side would have been an Int
      handleEither (Left "Error" :: Either String Int) `shouldBe` "Got an error: \"Error\""
    it "handles a Right value" $
      -- We specify that the 'Left' side would have been a String
      handleEither (Right 123 :: Either String Integer) `shouldBe` "Got a success value: 123"

