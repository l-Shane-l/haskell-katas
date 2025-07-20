module Katas.IO.IOCombinators where

{- | Implement when using >> (not if-then-else!)
when True (putStrLn "yes") prints "yes"
when False (putStrLn "yes") does nothing
-}
when' :: Bool -> IO () -> IO ()
when' = undefined

{- | Sequence a list of IO actions
sequence' [print 1, print 2] prints 1 then 2
-}
sequence' :: [IO a] -> IO [a]
sequence' = undefined

-- | Like sequence but ignore results (use >>)
sequence_' :: [IO a] -> IO ()
sequence_' = undefined

{- | Apply a function that returns IO to a list
Like map but for IO actions
-}
mapM' :: (a -> IO b) -> [a] -> IO [b]
mapM' = undefined

{- | The chapter's andThen function
This is (>>=) but implement it yourself for practice
-}
andThen :: IO a -> (a -> IO b) -> IO b
andThen = undefined

{- | Transform the value inside IO without do-notation
Use only fmap/<$>
-}
addLineNumbers :: IO String -> IO String
addLineNumbers ioStr = undefined

-- Should transform "Hello\nWorld" to "1: Hello\n2: World"
