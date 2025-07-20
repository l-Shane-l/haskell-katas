module Katas.IO.FileIOSpec (spec) where

import Control.Exception (IOException, try)
import Katas.IO.FileIO
import System.Directory
import System.IO.Temp
import Test.Hspec

spec :: Spec
spec = do
  describe "File IO Operations" $ do
    describe "copyFile" $ do
      it "copies file contents" $ do
        withSystemTempDirectory "test" $ \dir -> do
          let src = dir ++ "/source.txt"
              dst = dir ++ "/dest.txt"
          writeFile src "Hello, World!"
          copyFile src dst
          content <- readFile dst
          content `shouldBe` "Hello, World!"

    describe "countLines" $ do
      it "counts lines correctly" $ do
        withSystemTempFile "test.txt" $ \path handle -> do
          hPutStr handle "Line 1\nLine 2\nLine 3"
          hClose handle
          count <- countLines path
          count `shouldBe` 3

      it "handles empty file" $ do
        withSystemTempFile "test.txt" $ \path handle -> do
          hClose handle
          count <- countLines path
          count `shouldBe` 0

    describe "lazy IO behavior" $ do
      it "unsafeReadMany should fail with many files" $ do
        -- This test demonstrates the file handle exhaustion
        -- In practice, you might skip this test or use a smaller number
        pending -- "Demonstrates file handle exhaustion"
      it "safeProcessFiles handles many files" $ do
        withSystemTempDirectory "test" $ \dir -> do
          -- Create test files
          let files = [dir ++ "/file" ++ show n ++ ".txt" | n <- [1 .. 100]]
          mapM_ (\f -> writeFile f "content") files

          -- Process them safely
          output <- newIORef []
          safeProcessFiles (\s -> modifyIORef output (s :)) files
          result <- readIORef output
          length result `shouldBe` 100

    describe "combineFiles" $ do
      it "combines multiple files" $ do
        withSystemTempDirectory "test" $ \dir -> do
          let f1 = dir ++ "/1.txt"
              f2 = dir ++ "/2.txt"
          writeFile f1 "Hello"
          writeFile f2 "World"
          result <- combineFiles [f1, f2]
          result `shouldBe` "HelloWorld"

      it "handles missing files gracefully" $ do
        result <- combineFiles ["nonexistent.txt"]
        result `shouldBe` ""

-- Note: These tests use real file IO for demonstration
-- In production, you might mock the file system
