{-# LANGUAGE TypeApplications #-}

module Katas.TypeClasses.TypeApplicationsSpec (spec) where

import Katas.TypeClasses.TypeApplications
import Test.Hspec

spec :: Spec
spec = do
  describe "parseAs" $ do
    it "parses as Int" $
      parseAs @Int "42" `shouldBe` Just 42

    it "parses as Double" $
      parseAs @Double "3.14" `shouldBe` Just 3.14

    it "parses as Bool" $
      parseAs @Bool "True" `shouldBe` Just True

    it "returns Nothing for invalid input" $
      parseAs @Int "hello" `shouldBe` Nothing

  describe "typeName" $ do
    it "shows Int type name" $
      typeName @Int `shouldBe` "Int"

    it "shows String type name" $
      typeName @String `shouldBe` "[Char]"

    it "shows Maybe Int type name" $
      typeName @(Maybe Int) `shouldBe` "Maybe Int"

  describe "convert" $ do
    it "converts Int to Double" $
      convert @Int @Double 42 `shouldBe` 42.0

    it "converts Integer to Int" $
      convert @Integer @Int 100 `shouldBe` 100

