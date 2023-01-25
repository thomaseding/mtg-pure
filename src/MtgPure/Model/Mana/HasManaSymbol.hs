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
  manaSymbol = mempty

instance HasManaSymbol 'TyU where
  manaSymbol = mempty

instance HasManaSymbol 'TyB where
  manaSymbol = mempty

instance HasManaSymbol 'TyR where
  manaSymbol = mempty

instance HasManaSymbol 'TyG where
  manaSymbol = mempty
