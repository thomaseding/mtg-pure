{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.ToMana (
  ToMana (..),
) where

import safe Data.Kind (Type)
import safe MtgPure.Model.Mana.Mana (Mana (..))
import safe MtgPure.Model.Mana.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Mana.ManaType (ManaType (..))
import safe MtgPure.Model.Mana.Snow (Snow (..))
import safe MtgPure.Model.Variable (Var (..))

class ToMana (var :: Var) (mana :: Type) (snow :: Snow) (mt :: ManaType) | mana -> snow mt where
  toMana :: mana -> Mana var snow mt

instance ToMana var (Mana var snow a) snow a where
  toMana = id

instance ToMana var Int 'NonSnow 'MTGeneric where
  toMana = Mana

instance ToMana var (ManaSymbol 'MTWhite, Int) 'NonSnow 'MTWhite where
  toMana (W, n) = Mana n

instance ToMana var (ManaSymbol 'MTBlue, Int) 'NonSnow 'MTBlue where
  toMana (U, n) = Mana n

instance ToMana var (ManaSymbol 'MTBlack, Int) 'NonSnow 'MTBlack where
  toMana (B, n) = Mana n

instance ToMana var (ManaSymbol 'MTRed, Int) 'NonSnow 'MTRed where
  toMana (R, n) = Mana n

instance ToMana var (ManaSymbol 'MTGreen, Int) 'NonSnow 'MTGreen where
  toMana (G, n) = Mana n

instance ToMana var (ManaSymbol 'MTColorless, Int) 'NonSnow 'MTColorless where
  toMana (C, n) = Mana n

instance ToMana var (ManaSymbol 'MTSnow, Int) 'Snow 'MTGeneric where
  toMana (S, n) = Mana n

instance ToMana var (ManaSymbol 'MTHybridBG, Int) 'NonSnow 'MTHybridBG where
  toMana (BG, n) = Mana n

instance ToMana var (ManaSymbol 'MTWhite) 'NonSnow 'MTWhite where
  toMana W = toMana (W, 1 :: Int)

instance ToMana var (ManaSymbol 'MTBlue) 'NonSnow 'MTBlue where
  toMana U = toMana (U, 1 :: Int)

instance ToMana var (ManaSymbol 'MTBlack) 'NonSnow 'MTBlack where
  toMana B = toMana (B, 1 :: Int)

instance ToMana var (ManaSymbol 'MTRed) 'NonSnow 'MTRed where
  toMana R = toMana (R, 1 :: Int)

instance ToMana var (ManaSymbol 'MTGreen) 'NonSnow 'MTGreen where
  toMana G = toMana (G, 1 :: Int)

instance ToMana var (ManaSymbol 'MTColorless) 'NonSnow 'MTColorless where
  toMana C = toMana (C, 1 :: Int)

instance ToMana var (ManaSymbol 'MTSnow) 'Snow 'MTGeneric where
  toMana S = toMana (S, 1 :: Int)
