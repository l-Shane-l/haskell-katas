module Katas.Layer3_Toolkit.DataStructuresSpec (spec) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Katas.Layer3_Toolkit.DataStructures
import Test.Hspec

spec :: Spec
spec = do
  describe "Data Structure Katas" $ do
    it "calculates character frequency" $
      let result = charFrequency "hello world"
          expected = Map.fromList [('h', 1), ('e', 1), ('l', 3), ('o', 2), (' ', 1), ('w', 1), ('r', 1), ('d', 1)]
       in result `shouldBe` expected

    it "finds unique characters" $
      let result = uniqueChars "hello"
          expected = Set.fromList ['h', 'e', 'l', 'o']
       in result `shouldBe` expected
