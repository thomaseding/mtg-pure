{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.BasicLandType (
  BasicLandType (..),
) where

import safe Data.Typeable (Typeable)

data BasicLandType
  = Forest
  | Island
  | Mountain
  | Plains
  | Swamp
  deriving (Bounded, Enum, Eq, Ord, Show, Typeable)
