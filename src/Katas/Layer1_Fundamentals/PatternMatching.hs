module Katas.Layer1_Fundamentals.PatternMatching where

-- Goal: Master pattern matching on core Haskell data types.

-- | Use a 'case' expression to return "Empty" or "Non-empty".
describeList :: [a] -> String
describeList xs = undefined

-- | Use function-head pattern matching to get the first element of a 2-tuple.
getFirst :: (a, b) -> a
getFirst = undefined

-- | Use pattern matching on 'Maybe' to provide a default value.
unwrapMaybe :: Maybe a -> a -> a
unwrapMaybe = undefined

-- | Use pattern matching on 'Either' to convert it to a String.
handleEither :: (Show a) => (Show b) => Either a b -> String
handleEither = undefined
