module Katas.TypeClasses.BasicTypeClasses where

-- | Create a type class for things that can be "reversed"
class Reversible a where
  rev :: a -> a

-- | Make String an instance of Reversible
instance Reversible String where
  rev = undefined

-- | Make lists an instance of Reversible
instance Reversible [a] where
  rev = undefined

-- | Create a type class for things that have a default value
class Default a where
  def :: a

-- Make instances for common types
instance Default Int where
  def = undefined

instance Default String where
  def = undefined

instance Default Bool where
  def = undefined

instance Default [a] where
  def = undefined

instance Default (Maybe a) where
  def = undefined

-- | Use Default constraint in a function
fillDefaults :: (Default a) => [Maybe a] -> [a]
fillDefaults = undefined
