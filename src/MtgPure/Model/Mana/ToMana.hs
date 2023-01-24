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

instance ToMana var (ManaSymbol 'MTSnow, Int) 'Snow 'MTSnow where
  toMana (S, n) = Mana n

instance ToMana var (ManaSymbol 'MTHybridWU, Int) 'NonSnow 'MTHybridWU where
  toMana (WU, n) = Mana n

instance ToMana var (ManaSymbol 'MTHybridUB, Int) 'NonSnow 'MTHybridUB where
  toMana (UB, n) = Mana n

instance ToMana var (ManaSymbol 'MTHybridBR, Int) 'NonSnow 'MTHybridBR where
  toMana (BR, n) = Mana n

instance ToMana var (ManaSymbol 'MTHybridRG, Int) 'NonSnow 'MTHybridRG where
  toMana (RG, n) = Mana n

instance ToMana var (ManaSymbol 'MTHybridGW, Int) 'NonSnow 'MTHybridGW where
  toMana (GW, n) = Mana n

instance ToMana var (ManaSymbol 'MTHybridWB, Int) 'NonSnow 'MTHybridWB where
  toMana (WB, n) = Mana n

instance ToMana var (ManaSymbol 'MTHybridUR, Int) 'NonSnow 'MTHybridUR where
  toMana (UR, n) = Mana n

instance ToMana var (ManaSymbol 'MTHybridBG, Int) 'NonSnow 'MTHybridBG where
  toMana (BG, n) = Mana n

instance ToMana var (ManaSymbol 'MTHybridRW, Int) 'NonSnow 'MTHybridRW where
  toMana (RW, n) = Mana n

instance ToMana var (ManaSymbol 'MTHybridGU, Int) 'NonSnow 'MTHybridGU where
  toMana (GU, n) = Mana n

instance ToMana var (ManaSymbol 'MTHybridW2, Int) 'NonSnow 'MTHybridW2 where
  toMana (W2, n) = Mana n

instance ToMana var (ManaSymbol 'MTHybridU2, Int) 'NonSnow 'MTHybridU2 where
  toMana (U2, n) = Mana n

instance ToMana var (ManaSymbol 'MTHybridB2, Int) 'NonSnow 'MTHybridB2 where
  toMana (B2, n) = Mana n

instance ToMana var (ManaSymbol 'MTHybridR2, Int) 'NonSnow 'MTHybridR2 where
  toMana (R2, n) = Mana n

instance ToMana var (ManaSymbol 'MTHybridG2, Int) 'NonSnow 'MTHybridG2 where
  toMana (G2, n) = Mana n

instance ToMana var (ManaSymbol 'MTPhyrexianWhite, Int) 'NonSnow 'MTPhyrexianWhite where
  toMana (PW, n) = Mana n

instance ToMana var (ManaSymbol 'MTPhyrexianBlue, Int) 'NonSnow 'MTPhyrexianBlue where
  toMana (PU, n) = Mana n

instance ToMana var (ManaSymbol 'MTPhyrexianBlack, Int) 'NonSnow 'MTPhyrexianBlack where
  toMana (PB, n) = Mana n

instance ToMana var (ManaSymbol 'MTPhyrexianRed, Int) 'NonSnow 'MTPhyrexianRed where
  toMana (PR, n) = Mana n

instance ToMana var (ManaSymbol 'MTPhyrexianGreen, Int) 'NonSnow 'MTPhyrexianGreen where
  toMana (PG, n) = Mana n

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

instance ToMana var (ManaSymbol 'MTSnow) 'Snow 'MTSnow where
  toMana S = toMana (S, 1 :: Int)

instance ToMana var (ManaSymbol 'MTHybridWU) 'NonSnow 'MTHybridWU where
  toMana WU = toMana (WU, 1 :: Int)

instance ToMana var (ManaSymbol 'MTHybridUB) 'NonSnow 'MTHybridUB where
  toMana UB = toMana (UB, 1 :: Int)

instance ToMana var (ManaSymbol 'MTHybridBR) 'NonSnow 'MTHybridBR where
  toMana BR = toMana (BR, 1 :: Int)

instance ToMana var (ManaSymbol 'MTHybridRG) 'NonSnow 'MTHybridRG where
  toMana RG = toMana (RG, 1 :: Int)

instance ToMana var (ManaSymbol 'MTHybridGW) 'NonSnow 'MTHybridGW where
  toMana GW = toMana (GW, 1 :: Int)

instance ToMana var (ManaSymbol 'MTHybridWB) 'NonSnow 'MTHybridWB where
  toMana WB = toMana (WB, 1 :: Int)

instance ToMana var (ManaSymbol 'MTHybridUR) 'NonSnow 'MTHybridUR where
  toMana UR = toMana (UR, 1 :: Int)

instance ToMana var (ManaSymbol 'MTHybridBG) 'NonSnow 'MTHybridBG where
  toMana BG = toMana (BG, 1 :: Int)

instance ToMana var (ManaSymbol 'MTHybridRW) 'NonSnow 'MTHybridRW where
  toMana RW = toMana (RW, 1 :: Int)

instance ToMana var (ManaSymbol 'MTHybridGU) 'NonSnow 'MTHybridGU where
  toMana GU = toMana (GU, 1 :: Int)

instance ToMana var (ManaSymbol 'MTHybridW2) 'NonSnow 'MTHybridW2 where
  toMana W2 = toMana (W2, 1 :: Int)

instance ToMana var (ManaSymbol 'MTHybridU2) 'NonSnow 'MTHybridU2 where
  toMana U2 = toMana (U2, 1 :: Int)

instance ToMana var (ManaSymbol 'MTHybridB2) 'NonSnow 'MTHybridB2 where
  toMana B2 = toMana (B2, 1 :: Int)

instance ToMana var (ManaSymbol 'MTHybridR2) 'NonSnow 'MTHybridR2 where
  toMana R2 = toMana (R2, 1 :: Int)

instance ToMana var (ManaSymbol 'MTHybridG2) 'NonSnow 'MTHybridG2 where
  toMana G2 = toMana (G2, 1 :: Int)

instance ToMana var (ManaSymbol 'MTPhyrexianWhite) 'NonSnow 'MTPhyrexianWhite where
  toMana PW = toMana (PW, 1 :: Int)

instance ToMana var (ManaSymbol 'MTPhyrexianBlue) 'NonSnow 'MTPhyrexianBlue where
  toMana PU = toMana (PU, 1 :: Int)

instance ToMana var (ManaSymbol 'MTPhyrexianBlack) 'NonSnow 'MTPhyrexianBlack where
  toMana PB = toMana (PB, 1 :: Int)

instance ToMana var (ManaSymbol 'MTPhyrexianRed) 'NonSnow 'MTPhyrexianRed where
  toMana PR = toMana (PR, 1 :: Int)

instance ToMana var (ManaSymbol 'MTPhyrexianGreen) 'NonSnow 'MTPhyrexianGreen where
  toMana PG = toMana (PG, 1 :: Int)
