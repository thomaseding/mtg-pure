{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.ManaTypeToColor (
  ManaTypeToColor,
) where

import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.Mana.ManaType (ManaType (..))

type family ManaTypeToColor (mt :: ManaType) = (c :: Color) | c -> mt where
  ManaTypeToColor 'TyW = 'White
  ManaTypeToColor 'TyU = 'Blue
  ManaTypeToColor 'TyB = 'Black
  ManaTypeToColor 'TyR = 'Red
  ManaTypeToColor 'TyG = 'Green
