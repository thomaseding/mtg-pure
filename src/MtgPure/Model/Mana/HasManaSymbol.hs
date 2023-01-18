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

instance HasManaSymbol 'MTWhite where
  manaSymbol = mempty

instance HasManaSymbol 'MTBlue where
  manaSymbol = mempty

instance HasManaSymbol 'MTBlack where
  manaSymbol = mempty

instance HasManaSymbol 'MTRed where
  manaSymbol = mempty

instance HasManaSymbol 'MTGreen where
  manaSymbol = mempty
