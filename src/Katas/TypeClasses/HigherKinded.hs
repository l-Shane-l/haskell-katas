{-# LANGUAGE KindSignatures #-}

module Katas.TypeClasses.HigherKinded where

import Data.Kind (Type)

-- | Type class for containers that can be "compressed"
class Compress (f :: Type -> Type) where
  compress :: (Eq a) => f a -> f a

instance Compress [] where
  compress = undefined -- remove consecutive duplicates

instance Compress Maybe where
  compress = undefined -- Just stays Just, Nothing stays Nothing

-- | Type class for containers that support filtering
class Filterable (f :: Type -> Type) where
  filterF :: (a -> Bool) -> f a -> f a

instance Filterable [] where
  filterF = undefined

instance Filterable Maybe where
  filterF = undefined
