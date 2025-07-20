{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Katas.TypeClasses.TypeApplications where

import Data.Typeable
import Text.Read (readMaybe)

-- | Parse a string to any readable type (use type applications)
parseAs :: forall a. (Read a) => String -> Maybe a
parseAs = undefined

-- | Show the type name of a value
typeName :: forall a. (Typeable a) => String
typeName = undefined

-- | Convert between numeric types
convert :: forall a b. (Integral a, Num b) => a -> b
convert = undefined
