module Katas.Layer4_Apex.Abstractions where

-- Goal: Understand Functor for abstraction and IO for effects.

-- | 1. ABSTRACTION: Define a Tree data type and make it an instance of Functor.
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

instance Functor Tree where
  fmap = undefined -- Implement fmap for Tree

{- | 2. EFFECTS: Write a simple IO action that greets the user.
HINT: Use 'getLine' to read input and 'putStrLn' to print.
-}
greetUser :: IO ()
greetUser = undefined
