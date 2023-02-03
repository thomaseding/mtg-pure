{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.LandType (
  LandType (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.BasicLandType (BasicLandType)

data LandType :: Type where
  BasicLand :: BasicLandType -> LandType
  Desert :: LandType
  Gate :: LandType
  Lair :: LandType
  Locus :: LandType
  Mine :: LandType
  PowerPlant :: LandType
  Tower :: LandType
  Urzas :: LandType
  deriving (Eq, Ord, Show, Typeable)
