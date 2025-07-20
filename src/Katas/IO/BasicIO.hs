module Katas.IO.BasicIO where

{- | Echo user input with prefix
User types "Hello" -> prints "You said: Hello"
-}
echo :: IO ()
echo = undefined

{- | Read a number, print its square
Handle invalid input by printing error message
-}
squareInput :: IO ()
squareInput = undefined

{- | Combine multiple IO actions in sequence
Ask for first name, last name, then greet
-}
greetUser :: IO ()
greetUser = undefined

{- | Use fmap to transform IO String to IO Int
Should return Nothing for invalid input
-}
readInt :: IO (Maybe Int)
readInt = undefined
