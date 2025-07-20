module Katas.Layer2_Workhorse.DataStructuresSpec (spec) where

import qualified Data.List as List
import qualified Data.Set as Set
import Katas.Layer3_Toolkit.DataStructures
import Test.Hspec

spec :: Spec
spec = do
  describe "Data Structure Selection" $ do
    describe "Part 1: Choosing Right Structure" $ do
      it "counts word frequencies" $ do
        let text = "the cat in the hat"
        let freqs = List.sort $ wordFrequencies text
        freqs `shouldBe` [("cat", 1), ("hat", 1), ("in", 1), ("the", 2)]

      it "finds unique elements efficiently" $ do
        unique [3, 1, 4, 1, 5, 9, 2, 6, 5, 3] `shouldBe` [3, 1, 4, 5, 9, 2, 6]
      -- Should maintain order of first occurrence

      it "implements efficient queue" $ do
        let q0 = enqueue 1 $ enqueue 2 $ enqueue 3 emptyQueue
        case dequeue q0 of
          Just (1, q1) -> case dequeue q1 of
            Just (2, q2) -> case dequeue q2 of
              Just (3, q3) -> dequeue q3 `shouldBe` Nothing
              _ -> error "Wrong dequeue"
            _ -> error "Wrong dequeue"
          _ -> error "Wrong dequeue"

      it "provides O(1) random access" $ do
        let xs = [0 .. 1000]
        getNth 500 xs `shouldBe` Just 500
        getNth 1001 xs `shouldBe` Nothing

    describe "Part 2: Text and ByteString" $ do
      it "efficiently concatenates strings" $ do
        let strings = replicate 1000 "hello"
        length (efficientConcat strings) `shouldBe` 5000

      it "counts lines efficiently" $
        pending -- IO test
    describe "Part 3: Maps and Sets" $ do
      it "represents graphs" $ do
        let g0 = emptyGraph
            g1 = addEdge 'a' 'b' $ addEdge 'a' 'c' $ addEdge 'b' 'c' g0
        Set.fromList (neighbors 'a' g1) `shouldBe` Set.fromList ['b', 'c']
        Set.fromList (neighbors 'b' g1) `shouldBe` Set.fromList ['c']

      it "performs set operations" $ do
        intersection [1, 2, 3, 4] [3, 4, 5, 6] `shouldBe` [3, 4]
        intersection [1, 2] [3, 4] `shouldBe` []

    describe "Part 4: Sequences" $ do
      it "implements undo/redo" $ do
        let ur0 = emptyUndoRedo
            ur1 = doAction "edit1" ur0
            ur2 = doAction "edit2" ur1
        case undo ur2 of
          Just ("edit2", ur3) -> case redo ur3 of
            Just ("edit2", _) -> True `shouldBe` True
            _ -> error "Redo failed"
          _ -> error "Undo failed"

    describe "Part 5: Vectors" $ do
      it "multiplies matrices" $
        pending -- Requires matrix implementation
      it "calculates statistics efficiently" $ do
        let (mean, minVal, maxVal) = stats [1, 2, 3, 4, 5]
        mean `shouldBe` 3.0
        minVal `shouldBe` 1.0
        maxVal `shouldBe` 5.0

    describe "Part 6: Specialized Structures" $ do
      it "handles phone book efficiently" $ do
        let pb = addPhone 5551234 "Alice" $ addPhone 5555678 "Bob" emptyPhoneBook
        lookupPhone 5551234 pb `shouldBe` Just "Alice"

      it "implements cache with string keys" $ do
        let cache = cacheInsert "key1" "value1" emptyCache
        cacheLookup "key1" cache `shouldBe` Just "value1"

    describe "Part 8: Real-World Database" $ do
      it "inserts and queries by ID" $ do
        let db0 = emptyDatabase
            rec = Record 1 [("name", "Alice"), ("age", "30")]
            db1 = insertRecord rec db0
        queryById 1 db1 `shouldBe` Just rec
        queryById 2 db1 `shouldBe` Nothing

      it "queries by field" $ do
        let db =
              insertRecord (Record 1 [("name", "Alice"), ("age", "30")]) $
                insertRecord (Record 2 [("name", "Bob"), ("age", "30")]) $
                  emptyDatabase
        length (queryByField "age" "30" db) `shouldBe` 2

-- Helper functions that should be defined
emptyQueue :: Queue a
emptyQueue = undefined -- You implement

emptyGraph :: (Ord a) => Graph a
emptyGraph = undefined -- You implement

emptyUndoRedo :: UndoRedo a
emptyUndoRedo = undefined -- You implement

emptyPhoneBook :: PhoneBook
emptyPhoneBook = undefined -- You implement

lookupPhone :: Int -> PhoneBook -> Maybe String
lookupPhone = undefined -- You implement

emptyCache :: Cache a
emptyCache = undefined -- You implement

cacheLookup :: String -> Cache a -> Maybe a
cacheLookup = undefined -- You implement

emptyDatabase :: Database
emptyDatabase = undefined -- You implement

