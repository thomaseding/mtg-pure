{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ColorToManaType (
  ColorToManaType,
) where

import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.Mana.ManaType (ManaType (..))

type family ColorToManaType (c :: Color) = (mt :: ManaType) | mt -> c where
  ColorToManaType 'White = 'MTWhite
  ColorToManaType 'Blue = 'MTBlue
  ColorToManaType 'Black = 'MTBlack
  ColorToManaType 'Red = 'MTRed
  ColorToManaType 'Green = 'MTGreen
