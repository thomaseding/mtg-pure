{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.HasManaSymbol (
  HasManaSymbol (..),
) where

import safe MtgPure.Model.Mana.ManaSymbol (ManaSymbol)
import safe MtgPure.Model.Mana.ManaType (ManaType (..))

class HasManaSymbol a where
  manaSymbol :: ManaSymbol a

instance HasManaSymbol 'TyW where
  manaSymbol :: ManaSymbol 'TyW
  manaSymbol = mempty

instance HasManaSymbol 'TyU where
  manaSymbol :: ManaSymbol 'TyU
  manaSymbol = mempty

instance HasManaSymbol 'TyB where
  manaSymbol :: ManaSymbol 'TyB
  manaSymbol = mempty

instance HasManaSymbol 'TyR where
  manaSymbol :: ManaSymbol 'TyR
  manaSymbol = mempty

instance HasManaSymbol 'TyG where
  manaSymbol :: ManaSymbol 'TyG
  manaSymbol = mempty
