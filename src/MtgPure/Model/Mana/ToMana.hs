{-# LANGUAGE Safe #-}
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

instance ToMana var Int 'NonSnow 'Ty1 where
  toMana = Mana

instance ToMana var (ManaSymbol 'TyW, Int) 'NonSnow 'TyW where
  toMana (W, n) = Mana n

instance ToMana var (ManaSymbol 'TyU, Int) 'NonSnow 'TyU where
  toMana (U, n) = Mana n

instance ToMana var (ManaSymbol 'TyB, Int) 'NonSnow 'TyB where
  toMana (B, n) = Mana n

instance ToMana var (ManaSymbol 'TyR, Int) 'NonSnow 'TyR where
  toMana (R, n) = Mana n

instance ToMana var (ManaSymbol 'TyG, Int) 'NonSnow 'TyG where
  toMana (G, n) = Mana n

instance ToMana var (ManaSymbol 'TyC, Int) 'NonSnow 'TyC where
  toMana (C, n) = Mana n

instance ToMana var (ManaSymbol 'TyS, Int) 'Snow 'Ty1 where
  toMana (S, n) = Mana n

instance ToMana var (ManaSymbol 'TySW, Int) 'Snow 'TyW where
  toMana (SW, n) = Mana n

instance ToMana var (ManaSymbol 'TySU, Int) 'Snow 'TyU where
  toMana (SU, n) = Mana n

instance ToMana var (ManaSymbol 'TySB, Int) 'Snow 'TyB where
  toMana (SB, n) = Mana n

instance ToMana var (ManaSymbol 'TySR, Int) 'Snow 'TyR where
  toMana (SR, n) = Mana n

instance ToMana var (ManaSymbol 'TySG, Int) 'Snow 'TyG where
  toMana (SG, n) = Mana n

instance ToMana var (ManaSymbol 'TySC, Int) 'Snow 'TyC where
  toMana (SC, n) = Mana n

instance ToMana var (ManaSymbol 'TyWU, Int) 'NonSnow 'TyWU where
  toMana (WU, n) = Mana n

instance ToMana var (ManaSymbol 'TyUB, Int) 'NonSnow 'TyUB where
  toMana (UB, n) = Mana n

instance ToMana var (ManaSymbol 'TyBR, Int) 'NonSnow 'TyBR where
  toMana (BR, n) = Mana n

instance ToMana var (ManaSymbol 'TyRG, Int) 'NonSnow 'TyRG where
  toMana (RG, n) = Mana n

instance ToMana var (ManaSymbol 'TyGW, Int) 'NonSnow 'TyGW where
  toMana (GW, n) = Mana n

instance ToMana var (ManaSymbol 'TyWB, Int) 'NonSnow 'TyWB where
  toMana (WB, n) = Mana n

instance ToMana var (ManaSymbol 'TyUR, Int) 'NonSnow 'TyUR where
  toMana (UR, n) = Mana n

instance ToMana var (ManaSymbol 'TyBG, Int) 'NonSnow 'TyBG where
  toMana (BG, n) = Mana n

instance ToMana var (ManaSymbol 'TyRW, Int) 'NonSnow 'TyRW where
  toMana (RW, n) = Mana n

instance ToMana var (ManaSymbol 'TyGU, Int) 'NonSnow 'TyGU where
  toMana (GU, n) = Mana n

instance ToMana var (ManaSymbol 'TyW2, Int) 'NonSnow 'TyW2 where
  toMana (W2, n) = Mana n

instance ToMana var (ManaSymbol 'TyU2, Int) 'NonSnow 'TyU2 where
  toMana (U2, n) = Mana n

instance ToMana var (ManaSymbol 'TyB2, Int) 'NonSnow 'TyB2 where
  toMana (B2, n) = Mana n

instance ToMana var (ManaSymbol 'TyR2, Int) 'NonSnow 'TyR2 where
  toMana (R2, n) = Mana n

instance ToMana var (ManaSymbol 'TyG2, Int) 'NonSnow 'TyG2 where
  toMana (G2, n) = Mana n

instance ToMana var (ManaSymbol 'TyC2, Int) 'NonSnow 'TyC2 where
  toMana (C2, n) = Mana n

instance ToMana var (ManaSymbol 'TyPW, Int) 'NonSnow 'TyPW where
  toMana (PW, n) = Mana n

instance ToMana var (ManaSymbol 'TyPU, Int) 'NonSnow 'TyPU where
  toMana (PU, n) = Mana n

instance ToMana var (ManaSymbol 'TyPB, Int) 'NonSnow 'TyPB where
  toMana (PB, n) = Mana n

instance ToMana var (ManaSymbol 'TyPR, Int) 'NonSnow 'TyPR where
  toMana (PR, n) = Mana n

instance ToMana var (ManaSymbol 'TyPG, Int) 'NonSnow 'TyPG where
  toMana (PG, n) = Mana n

instance ToMana var (ManaSymbol 'TyPC, Int) 'NonSnow 'TyPC where
  toMana (PC, n) = Mana n

instance ToMana var (ManaSymbol 'TyW) 'NonSnow 'TyW where
  toMana W = toMana (W, 1 :: Int)

instance ToMana var (ManaSymbol 'TyU) 'NonSnow 'TyU where
  toMana U = toMana (U, 1 :: Int)

instance ToMana var (ManaSymbol 'TyB) 'NonSnow 'TyB where
  toMana B = toMana (B, 1 :: Int)

instance ToMana var (ManaSymbol 'TyR) 'NonSnow 'TyR where
  toMana R = toMana (R, 1 :: Int)

instance ToMana var (ManaSymbol 'TyG) 'NonSnow 'TyG where
  toMana G = toMana (G, 1 :: Int)

instance ToMana var (ManaSymbol 'TyC) 'NonSnow 'TyC where
  toMana C = toMana (C, 1 :: Int)

instance ToMana var (ManaSymbol 'TyS) 'Snow 'Ty1 where
  toMana S = toMana (S, 1 :: Int)

instance ToMana var (ManaSymbol 'TyWU) 'NonSnow 'TyWU where
  toMana WU = toMana (WU, 1 :: Int)

instance ToMana var (ManaSymbol 'TyUB) 'NonSnow 'TyUB where
  toMana UB = toMana (UB, 1 :: Int)

instance ToMana var (ManaSymbol 'TyBR) 'NonSnow 'TyBR where
  toMana BR = toMana (BR, 1 :: Int)

instance ToMana var (ManaSymbol 'TyRG) 'NonSnow 'TyRG where
  toMana RG = toMana (RG, 1 :: Int)

instance ToMana var (ManaSymbol 'TyGW) 'NonSnow 'TyGW where
  toMana GW = toMana (GW, 1 :: Int)

instance ToMana var (ManaSymbol 'TyWB) 'NonSnow 'TyWB where
  toMana WB = toMana (WB, 1 :: Int)

instance ToMana var (ManaSymbol 'TyUR) 'NonSnow 'TyUR where
  toMana UR = toMana (UR, 1 :: Int)

instance ToMana var (ManaSymbol 'TyBG) 'NonSnow 'TyBG where
  toMana BG = toMana (BG, 1 :: Int)

instance ToMana var (ManaSymbol 'TyRW) 'NonSnow 'TyRW where
  toMana RW = toMana (RW, 1 :: Int)

instance ToMana var (ManaSymbol 'TyGU) 'NonSnow 'TyGU where
  toMana GU = toMana (GU, 1 :: Int)

instance ToMana var (ManaSymbol 'TyW2) 'NonSnow 'TyW2 where
  toMana W2 = toMana (W2, 1 :: Int)

instance ToMana var (ManaSymbol 'TyU2) 'NonSnow 'TyU2 where
  toMana U2 = toMana (U2, 1 :: Int)

instance ToMana var (ManaSymbol 'TyB2) 'NonSnow 'TyB2 where
  toMana B2 = toMana (B2, 1 :: Int)

instance ToMana var (ManaSymbol 'TyR2) 'NonSnow 'TyR2 where
  toMana R2 = toMana (R2, 1 :: Int)

instance ToMana var (ManaSymbol 'TyC2) 'NonSnow 'TyC2 where
  toMana C2 = toMana (C2, 1 :: Int)

instance ToMana var (ManaSymbol 'TyG2) 'NonSnow 'TyG2 where
  toMana G2 = toMana (G2, 1 :: Int)

instance ToMana var (ManaSymbol 'TyPW) 'NonSnow 'TyPW where
  toMana PW = toMana (PW, 1 :: Int)

instance ToMana var (ManaSymbol 'TyPU) 'NonSnow 'TyPU where
  toMana PU = toMana (PU, 1 :: Int)

instance ToMana var (ManaSymbol 'TyPB) 'NonSnow 'TyPB where
  toMana PB = toMana (PB, 1 :: Int)

instance ToMana var (ManaSymbol 'TyPR) 'NonSnow 'TyPR where
  toMana PR = toMana (PR, 1 :: Int)

instance ToMana var (ManaSymbol 'TyPG) 'NonSnow 'TyPG where
  toMana PG = toMana (PG, 1 :: Int)

instance ToMana var (ManaSymbol 'TyPC) 'NonSnow 'TyPC where
  toMana PC = toMana (PC, 1 :: Int)
