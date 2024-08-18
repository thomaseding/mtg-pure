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
  toMana :: Mana var snow a -> Mana var snow a
  toMana = id

instance ToMana var Int 'NonSnow 'Ty1 where
  toMana :: Int -> Mana var 'NonSnow 'Ty1
  toMana = Mana

instance ToMana var (ManaSymbol 'TyW, Int) 'NonSnow 'TyW where
  toMana :: (ManaSymbol 'TyW, Int) -> Mana var 'NonSnow 'TyW
  toMana (W, n) = Mana n

instance ToMana var (ManaSymbol 'TyU, Int) 'NonSnow 'TyU where
  toMana :: (ManaSymbol 'TyU, Int) -> Mana var 'NonSnow 'TyU
  toMana (U, n) = Mana n

instance ToMana var (ManaSymbol 'TyB, Int) 'NonSnow 'TyB where
  toMana :: (ManaSymbol 'TyB, Int) -> Mana var 'NonSnow 'TyB
  toMana (B, n) = Mana n

instance ToMana var (ManaSymbol 'TyR, Int) 'NonSnow 'TyR where
  toMana :: (ManaSymbol 'TyR, Int) -> Mana var 'NonSnow 'TyR
  toMana (R, n) = Mana n

instance ToMana var (ManaSymbol 'TyG, Int) 'NonSnow 'TyG where
  toMana :: (ManaSymbol 'TyG, Int) -> Mana var 'NonSnow 'TyG
  toMana (G, n) = Mana n

instance ToMana var (ManaSymbol 'TyC, Int) 'NonSnow 'TyC where
  toMana :: (ManaSymbol 'TyC, Int) -> Mana var 'NonSnow 'TyC
  toMana (C, n) = Mana n

instance ToMana var (ManaSymbol 'TyS, Int) 'Snow 'Ty1 where
  toMana :: (ManaSymbol 'TyS, Int) -> Mana var 'Snow 'Ty1
  toMana (S, n) = Mana n

instance ToMana var (ManaSymbol 'TySW, Int) 'Snow 'TyW where
  toMana :: (ManaSymbol 'TySW, Int) -> Mana var 'Snow 'TyW
  toMana (SW, n) = Mana n

instance ToMana var (ManaSymbol 'TySU, Int) 'Snow 'TyU where
  toMana :: (ManaSymbol 'TySU, Int) -> Mana var 'Snow 'TyU
  toMana (SU, n) = Mana n

instance ToMana var (ManaSymbol 'TySB, Int) 'Snow 'TyB where
  toMana :: (ManaSymbol 'TySB, Int) -> Mana var 'Snow 'TyB
  toMana (SB, n) = Mana n

instance ToMana var (ManaSymbol 'TySR, Int) 'Snow 'TyR where
  toMana :: (ManaSymbol 'TySR, Int) -> Mana var 'Snow 'TyR
  toMana (SR, n) = Mana n

instance ToMana var (ManaSymbol 'TySG, Int) 'Snow 'TyG where
  toMana :: (ManaSymbol 'TySG, Int) -> Mana var 'Snow 'TyG
  toMana (SG, n) = Mana n

instance ToMana var (ManaSymbol 'TySC, Int) 'Snow 'TyC where
  toMana :: (ManaSymbol 'TySC, Int) -> Mana var 'Snow 'TyC
  toMana (SC, n) = Mana n

instance ToMana var (ManaSymbol 'TyWU, Int) 'NonSnow 'TyWU where
  toMana :: (ManaSymbol 'TyWU, Int) -> Mana var 'NonSnow 'TyWU
  toMana (WU, n) = Mana n

instance ToMana var (ManaSymbol 'TyUB, Int) 'NonSnow 'TyUB where
  toMana :: (ManaSymbol 'TyUB, Int) -> Mana var 'NonSnow 'TyUB
  toMana (UB, n) = Mana n

instance ToMana var (ManaSymbol 'TyBR, Int) 'NonSnow 'TyBR where
  toMana :: (ManaSymbol 'TyBR, Int) -> Mana var 'NonSnow 'TyBR
  toMana (BR, n) = Mana n

instance ToMana var (ManaSymbol 'TyRG, Int) 'NonSnow 'TyRG where
  toMana :: (ManaSymbol 'TyRG, Int) -> Mana var 'NonSnow 'TyRG
  toMana (RG, n) = Mana n

instance ToMana var (ManaSymbol 'TyGW, Int) 'NonSnow 'TyGW where
  toMana :: (ManaSymbol 'TyGW, Int) -> Mana var 'NonSnow 'TyGW
  toMana (GW, n) = Mana n

instance ToMana var (ManaSymbol 'TyWB, Int) 'NonSnow 'TyWB where
  toMana :: (ManaSymbol 'TyWB, Int) -> Mana var 'NonSnow 'TyWB
  toMana (WB, n) = Mana n

instance ToMana var (ManaSymbol 'TyUR, Int) 'NonSnow 'TyUR where
  toMana :: (ManaSymbol 'TyUR, Int) -> Mana var 'NonSnow 'TyUR
  toMana (UR, n) = Mana n

instance ToMana var (ManaSymbol 'TyBG, Int) 'NonSnow 'TyBG where
  toMana :: (ManaSymbol 'TyBG, Int) -> Mana var 'NonSnow 'TyBG
  toMana (BG, n) = Mana n

instance ToMana var (ManaSymbol 'TyRW, Int) 'NonSnow 'TyRW where
  toMana :: (ManaSymbol 'TyRW, Int) -> Mana var 'NonSnow 'TyRW
  toMana (RW, n) = Mana n

instance ToMana var (ManaSymbol 'TyGU, Int) 'NonSnow 'TyGU where
  toMana :: (ManaSymbol 'TyGU, Int) -> Mana var 'NonSnow 'TyGU
  toMana (GU, n) = Mana n

instance ToMana var (ManaSymbol 'TyW2, Int) 'NonSnow 'TyW2 where
  toMana :: (ManaSymbol 'TyW2, Int) -> Mana var 'NonSnow 'TyW2
  toMana (W2, n) = Mana n

instance ToMana var (ManaSymbol 'TyU2, Int) 'NonSnow 'TyU2 where
  toMana :: (ManaSymbol 'TyU2, Int) -> Mana var 'NonSnow 'TyU2
  toMana (U2, n) = Mana n

instance ToMana var (ManaSymbol 'TyB2, Int) 'NonSnow 'TyB2 where
  toMana :: (ManaSymbol 'TyB2, Int) -> Mana var 'NonSnow 'TyB2
  toMana (B2, n) = Mana n

instance ToMana var (ManaSymbol 'TyR2, Int) 'NonSnow 'TyR2 where
  toMana :: (ManaSymbol 'TyR2, Int) -> Mana var 'NonSnow 'TyR2
  toMana (R2, n) = Mana n

instance ToMana var (ManaSymbol 'TyG2, Int) 'NonSnow 'TyG2 where
  toMana :: (ManaSymbol 'TyG2, Int) -> Mana var 'NonSnow 'TyG2
  toMana (G2, n) = Mana n

instance ToMana var (ManaSymbol 'TyC2, Int) 'NonSnow 'TyC2 where
  toMana :: (ManaSymbol 'TyC2, Int) -> Mana var 'NonSnow 'TyC2
  toMana (C2, n) = Mana n

instance ToMana var (ManaSymbol 'TyPW, Int) 'NonSnow 'TyPW where
  toMana :: (ManaSymbol 'TyPW, Int) -> Mana var 'NonSnow 'TyPW
  toMana (PW, n) = Mana n

instance ToMana var (ManaSymbol 'TyPU, Int) 'NonSnow 'TyPU where
  toMana :: (ManaSymbol 'TyPU, Int) -> Mana var 'NonSnow 'TyPU
  toMana (PU, n) = Mana n

instance ToMana var (ManaSymbol 'TyPB, Int) 'NonSnow 'TyPB where
  toMana :: (ManaSymbol 'TyPB, Int) -> Mana var 'NonSnow 'TyPB
  toMana (PB, n) = Mana n

instance ToMana var (ManaSymbol 'TyPR, Int) 'NonSnow 'TyPR where
  toMana :: (ManaSymbol 'TyPR, Int) -> Mana var 'NonSnow 'TyPR
  toMana (PR, n) = Mana n

instance ToMana var (ManaSymbol 'TyPG, Int) 'NonSnow 'TyPG where
  toMana :: (ManaSymbol 'TyPG, Int) -> Mana var 'NonSnow 'TyPG
  toMana (PG, n) = Mana n

instance ToMana var (ManaSymbol 'TyPC, Int) 'NonSnow 'TyPC where
  toMana :: (ManaSymbol 'TyPC, Int) -> Mana var 'NonSnow 'TyPC
  toMana (PC, n) = Mana n

instance ToMana var (ManaSymbol 'TyW) 'NonSnow 'TyW where
  toMana :: ManaSymbol 'TyW -> Mana var 'NonSnow 'TyW
  toMana W = toMana (W, 1 :: Int)

instance ToMana var (ManaSymbol 'TyU) 'NonSnow 'TyU where
  toMana :: ManaSymbol 'TyU -> Mana var 'NonSnow 'TyU
  toMana U = toMana (U, 1 :: Int)

instance ToMana var (ManaSymbol 'TyB) 'NonSnow 'TyB where
  toMana :: ManaSymbol 'TyB -> Mana var 'NonSnow 'TyB
  toMana B = toMana (B, 1 :: Int)

instance ToMana var (ManaSymbol 'TyR) 'NonSnow 'TyR where
  toMana :: ManaSymbol 'TyR -> Mana var 'NonSnow 'TyR
  toMana R = toMana (R, 1 :: Int)

instance ToMana var (ManaSymbol 'TyG) 'NonSnow 'TyG where
  toMana :: ManaSymbol 'TyG -> Mana var 'NonSnow 'TyG
  toMana G = toMana (G, 1 :: Int)

instance ToMana var (ManaSymbol 'TyC) 'NonSnow 'TyC where
  toMana :: ManaSymbol 'TyC -> Mana var 'NonSnow 'TyC
  toMana C = toMana (C, 1 :: Int)

instance ToMana var (ManaSymbol 'TyS) 'Snow 'Ty1 where
  toMana :: ManaSymbol 'TyS -> Mana var 'Snow 'Ty1
  toMana S = toMana (S, 1 :: Int)

instance ToMana var (ManaSymbol 'TyWU) 'NonSnow 'TyWU where
  toMana :: ManaSymbol 'TyWU -> Mana var 'NonSnow 'TyWU
  toMana WU = toMana (WU, 1 :: Int)

instance ToMana var (ManaSymbol 'TyUB) 'NonSnow 'TyUB where
  toMana :: ManaSymbol 'TyUB -> Mana var 'NonSnow 'TyUB
  toMana UB = toMana (UB, 1 :: Int)

instance ToMana var (ManaSymbol 'TyBR) 'NonSnow 'TyBR where
  toMana :: ManaSymbol 'TyBR -> Mana var 'NonSnow 'TyBR
  toMana BR = toMana (BR, 1 :: Int)

instance ToMana var (ManaSymbol 'TyRG) 'NonSnow 'TyRG where
  toMana :: ManaSymbol 'TyRG -> Mana var 'NonSnow 'TyRG
  toMana RG = toMana (RG, 1 :: Int)

instance ToMana var (ManaSymbol 'TyGW) 'NonSnow 'TyGW where
  toMana :: ManaSymbol 'TyGW -> Mana var 'NonSnow 'TyGW
  toMana GW = toMana (GW, 1 :: Int)

instance ToMana var (ManaSymbol 'TyWB) 'NonSnow 'TyWB where
  toMana :: ManaSymbol 'TyWB -> Mana var 'NonSnow 'TyWB
  toMana WB = toMana (WB, 1 :: Int)

instance ToMana var (ManaSymbol 'TyUR) 'NonSnow 'TyUR where
  toMana :: ManaSymbol 'TyUR -> Mana var 'NonSnow 'TyUR
  toMana UR = toMana (UR, 1 :: Int)

instance ToMana var (ManaSymbol 'TyBG) 'NonSnow 'TyBG where
  toMana :: ManaSymbol 'TyBG -> Mana var 'NonSnow 'TyBG
  toMana BG = toMana (BG, 1 :: Int)

instance ToMana var (ManaSymbol 'TyRW) 'NonSnow 'TyRW where
  toMana :: ManaSymbol 'TyRW -> Mana var 'NonSnow 'TyRW
  toMana RW = toMana (RW, 1 :: Int)

instance ToMana var (ManaSymbol 'TyGU) 'NonSnow 'TyGU where
  toMana :: ManaSymbol 'TyGU -> Mana var 'NonSnow 'TyGU
  toMana GU = toMana (GU, 1 :: Int)

instance ToMana var (ManaSymbol 'TyW2) 'NonSnow 'TyW2 where
  toMana :: ManaSymbol 'TyW2 -> Mana var 'NonSnow 'TyW2
  toMana W2 = toMana (W2, 1 :: Int)

instance ToMana var (ManaSymbol 'TyU2) 'NonSnow 'TyU2 where
  toMana :: ManaSymbol 'TyU2 -> Mana var 'NonSnow 'TyU2
  toMana U2 = toMana (U2, 1 :: Int)

instance ToMana var (ManaSymbol 'TyB2) 'NonSnow 'TyB2 where
  toMana :: ManaSymbol 'TyB2 -> Mana var 'NonSnow 'TyB2
  toMana B2 = toMana (B2, 1 :: Int)

instance ToMana var (ManaSymbol 'TyR2) 'NonSnow 'TyR2 where
  toMana :: ManaSymbol 'TyR2 -> Mana var 'NonSnow 'TyR2
  toMana R2 = toMana (R2, 1 :: Int)

instance ToMana var (ManaSymbol 'TyC2) 'NonSnow 'TyC2 where
  toMana :: ManaSymbol 'TyC2 -> Mana var 'NonSnow 'TyC2
  toMana C2 = toMana (C2, 1 :: Int)

instance ToMana var (ManaSymbol 'TyG2) 'NonSnow 'TyG2 where
  toMana :: ManaSymbol 'TyG2 -> Mana var 'NonSnow 'TyG2
  toMana G2 = toMana (G2, 1 :: Int)

instance ToMana var (ManaSymbol 'TyPW) 'NonSnow 'TyPW where
  toMana :: ManaSymbol 'TyPW -> Mana var 'NonSnow 'TyPW
  toMana PW = toMana (PW, 1 :: Int)

instance ToMana var (ManaSymbol 'TyPU) 'NonSnow 'TyPU where
  toMana :: ManaSymbol 'TyPU -> Mana var 'NonSnow 'TyPU
  toMana PU = toMana (PU, 1 :: Int)

instance ToMana var (ManaSymbol 'TyPB) 'NonSnow 'TyPB where
  toMana :: ManaSymbol 'TyPB -> Mana var 'NonSnow 'TyPB
  toMana PB = toMana (PB, 1 :: Int)

instance ToMana var (ManaSymbol 'TyPR) 'NonSnow 'TyPR where
  toMana :: ManaSymbol 'TyPR -> Mana var 'NonSnow 'TyPR
  toMana PR = toMana (PR, 1 :: Int)

instance ToMana var (ManaSymbol 'TyPG) 'NonSnow 'TyPG where
  toMana :: ManaSymbol 'TyPG -> Mana var 'NonSnow 'TyPG
  toMana PG = toMana (PG, 1 :: Int)

instance ToMana var (ManaSymbol 'TyPC) 'NonSnow 'TyPC where
  toMana :: ManaSymbol 'TyPC -> Mana var 'NonSnow 'TyPC
  toMana PC = toMana (PC, 1 :: Int)
