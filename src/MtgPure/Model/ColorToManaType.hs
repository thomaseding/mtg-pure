{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ColorToManaType (
  ColorToManaType,
) where

import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.Mana.ManaType (ManaType (..))

type family ColorToManaType (c :: Color) = (mt :: ManaType) | mt -> c where
  ColorToManaType 'White = 'TyW
  ColorToManaType 'Blue = 'TyU
  ColorToManaType 'Black = 'TyB
  ColorToManaType 'Red = 'TyR
  ColorToManaType 'Green = 'TyG
