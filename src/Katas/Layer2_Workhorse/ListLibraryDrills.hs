module Katas.Layer2_Workhorse.ListLibraryDrills where

import Data.List
import Data.Ord (comparing)

-- Goal: Achieve total recall of the standard list library.
-- For each 'undefined', fill in the correct function call to produce the desired result.

-- Drill 1: Basic Transformations & Sublists
kataMap = undefined -- map (+10) [1,2,3]
kataFilter = undefined -- filter even [1..10]
kataSortBy = undefined -- sortBy (comparing length) ["a", "cccc", "bb"]
kataTake = undefined -- take 3 [1..10]
kataDropWhile = undefined -- dropWhile (< 5) [1,2,3,6,4,5]

-- Drill 2: Folds & Aggregations
kataFoldl = undefined -- foldl' (+) 0 [1..5]
kataSum = undefined -- sum [1..5]
kataAll = undefined -- all even [2,4,6]
kataAny = undefined -- any odd [2,4,5,6]
kataMaximum = undefined -- maximum [5,1,9,3,2]

-- Drill 3: Searching, Splitting & Grouping
kataFind = undefined -- find (> 5) [1..10]
kataPartition = undefined -- partition even [1..10]
kataSpan = undefined -- span (< 5) [1,2,3,6,4,5]
kataGroupBy = undefined -- groupBy (\x y -> even x == even y) [1,3,2,4,6,7,5]
kataNub = undefined -- nub [1,2,1,3,4,2,3]

-- Drill 4: Zipping
kataZip = undefined -- zip [1,2,3] ['a','b','c','d']
kataZipWith = undefined -- zipWith (*) [1,2,3] [10,100,1000]
