module Katas.DataTypes.BasicTypes where

-- | Simple product type for 2D points
data Point = Point Double Double
  deriving (Eq, Show)

-- | Create a point from x,y coordinates
mkPoint :: Double -> Double -> Point
mkPoint = undefined

-- | Get distance from origin
distanceFromOrigin :: Point -> Double
distanceFromOrigin = undefined

-- | Person with record syntax
data Person = Person
  { firstName :: String
  , lastName :: String
  , age :: Int
  }
  deriving (Eq, Show)

-- | Smart constructor that validates age
mkPerson :: String -> String -> Int -> Maybe Person
mkPerson = undefined

-- | Birthday function using record update syntax
birthday :: Person -> Person
birthday = undefined

-- | Get full name
fullName :: Person -> String
fullName = undefined
