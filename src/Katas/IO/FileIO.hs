module Katas.IO.FileIO where

import System.IO

{- | Copy file contents (the chapter's main example)
Use readFile and writeFile
-}
copyFile :: FilePath -> FilePath -> IO ()
copyFile src dst = undefined

{- | Read file and count lines using fmap
Don't use do-notation, practice with fmap/<$>
-}
countLines :: FilePath -> IO Int
countLines path = undefined

{- | DANGER: This should crash with "too many open files"
Read many files lazily (demonstrates the bug)
-}
unsafeReadMany :: [FilePath] -> IO [String]
unsafeReadMany paths = undefined

{- | SAFE: Process files one at a time
Use mapM_ to avoid the lazy IO bug
-}
safeProcessFiles :: (String -> IO ()) -> [FilePath] -> IO ()
safeProcessFiles process paths = undefined

{- | Combine contents of multiple files
Should handle missing files gracefully
-}
combineFiles :: [FilePath] -> IO String
combineFiles paths = undefined
