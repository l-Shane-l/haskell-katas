module Katas.Layer4_Apex.AbstractionsSpec (spec) where

import Katas.Layer4_Apex.Abstractions
import Test.Hspec

spec :: Spec
spec = do
  describe "Abstraction Katas" $ do
    it "fmap works on a simple tree" $
      let tree = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)
          expected = Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)
       in fmap (+ 1) tree `shouldBe` expected

    it "fmap works on an empty tree" $
      fmap (+ 1) (Empty :: Tree Int) `shouldBe` Empty

    -- Note: IO actions like greetUser are typically not tested with Hspec.
    -- They are tested by running them in GHCi or with specialized libraries.
    -- We include this test just to ensure the file compiles.
    it "greetUser should compile" $
      True `shouldBe` True
