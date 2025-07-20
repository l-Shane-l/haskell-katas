module Katas.DataTypes.SumTypes where

-- | Simple enum-style sum type
data Direction = North | South | East | West
  deriving (Eq, Show)

-- | Turn a direction 90 degrees clockwise
turnClockwise :: Direction -> Direction
turnClockwise = undefined

-- | Sum of products for shapes
data Shape
  = Circle Double
  | Rectangle Double Double
  | Triangle Double Double Double
  deriving (Eq, Show)

-- | Calculate area of a shape
area :: Shape -> Double
area = undefined

-- | Calculate perimeter of a shape
perimeter :: Shape -> Double
perimeter = undefined

-- | Contact info with multiple options
data ContactInfo
  = Email String
  | Phone String
  | Address String String String -- street, city, zip
  deriving (Eq, Show)

-- | Format contact info for display
formatContact :: ContactInfo -> String
formatContact = undefined
