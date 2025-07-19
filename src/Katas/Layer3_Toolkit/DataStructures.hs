module Katas.Layer3_Toolkit.DataStructures where

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Goal: Use the right data structure for the job.

{- | Count the frequency of each character in a string.
HINT: Fold over the string, inserting/updating a Map.
-}
charFrequency :: String -> Map Char Int
charFrequency = undefined

{- | Find all the unique characters in a string.
HINT: The easiest way is to convert the list to a Set.
-}
uniqueChars :: String -> Set Char
uniqueChars = undefined
