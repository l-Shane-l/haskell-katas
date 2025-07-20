{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Katas.TypeClasses.NewtypeInstances where

-- | Email with validation
newtype Email = Email {getEmail :: String}
  deriving (Eq, Show)

-- Smart constructor
mkEmail :: String -> Maybe Email
mkEmail = undefined

-- | Age with bounds checking
newtype Age = Age {getAge :: Int}
  deriving (Eq, Ord, Show)

mkAge :: Int -> Maybe Age
mkAge = undefined

-- | Money type with Num instance (works in cents)
newtype USD = USD {getCents :: Int}
  deriving (Eq, Ord, Show)

instance Num USD where
  (+) = undefined
  (*) = undefined -- This should work with cents/dollars properly
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate = undefined

-- Helper to create USD from dollars
dollars :: Int -> USD
dollars = undefined

-- Show as dollars
showDollars :: USD -> String
showDollars = undefined
