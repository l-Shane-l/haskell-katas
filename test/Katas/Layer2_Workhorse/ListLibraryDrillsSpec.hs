module Katas.Layer2_Workhorse.ListLibraryDrillsSpec (spec) where

import Katas.Layer2_Workhorse.ListLibraryDrills
import Test.Hspec

spec :: Spec
spec = do
  describe "List Library Drills" $ do
    it "kataMap" $ kataMap `shouldBe` [11, 12, 13]
    it "kataFilter" $ kataFilter `shouldBe` [2, 4, 6, 8, 10]
    it "kataSortBy" $ kataSortBy `shouldBe` ["a", "bb", "cccc"]
    it "kataTake" $ kataTake `shouldBe` [1, 2, 3]
    it "kataDropWhile" $ kataDropWhile `shouldBe` [6, 4, 5]
    it "kataFoldl" $ kataFoldl `shouldBe` 15
    it "kataSum" $ kataSum `shouldBe` 15
    it "kataAll" $ kataAll `shouldBe` True
    it "kataAny" $ kataAny `shouldBe` True
    it "kataMaximum" $ kataMaximum `shouldBe` 9
    it "kataFind" $ kataFind `shouldBe` Just 6
    it "kataPartition" $ kataPartition `shouldBe` ([2, 4, 6, 8, 10], [1, 3, 5, 7, 9])
    it "kataSpan" $ kataSpan `shouldBe` ([1, 2, 3], [6, 4, 5])
    it "kataGroupBy" $ kataGroupBy `shouldBe` [[1, 3], [2, 4, 6], [7, 5]]
    it "kataNub" $ kataNub `shouldBe` [1, 2, 3, 4]
    it "kataZip" $ kataZip `shouldBe` [(1, 'a'), (2, 'b'), (3, 'c')]
    it "kataZipWith" $ kataZipWith `shouldBe` [10, 200, 3000]
