{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.ManaType (
  ManaType (..),
  SManaType (..),
  IsManaType (..),
) where

import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Mana.Snow (Snow (..))

data ManaType
  = TyW
  | TyU
  | TyB
  | TyR
  | TyG
  | TyC
  | Ty1
  | TyS
  | TyWU
  | TyUB
  | TyBR
  | TyRG
  | TyGW
  | TyWB
  | TyUR
  | TyBG
  | TyRW
  | TyGU
  | TyW2
  | TyU2
  | TyB2
  | TyR2
  | TyG2
  | TyPW
  | TyPU
  | TyPB
  | TyPR
  | TyPG
  | TyPC
  deriving (Eq, Ord, Typeable)

data SManaType (snow :: Snow) (mt :: ManaType) where
  STyW :: SManaType 'NonSnow 'TyW
  STyU :: SManaType 'NonSnow 'TyU
  STyB :: SManaType 'NonSnow 'TyB
  STyR :: SManaType 'NonSnow 'TyR
  STyG :: SManaType 'NonSnow 'TyG
  STyC :: SManaType 'NonSnow 'TyC
  STy1 :: SManaType 'NonSnow 'Ty1
  STyS :: SManaType 'Snow 'TyS
  STyBG :: SManaType 'NonSnow 'TyBG
  STyPW :: SManaType 'NonSnow 'TyPW
  STyPU :: SManaType 'NonSnow 'TyPU
  STyPB :: SManaType 'NonSnow 'TyPB
  STyPR :: SManaType 'NonSnow 'TyPR
  STyPG :: SManaType 'NonSnow 'TyPG
  STyPC :: SManaType 'NonSnow 'TyPC

class IsManaType (snow :: Snow) (mt :: ManaType) where
  singManaType :: SManaType snow mt

instance IsManaType 'NonSnow 'TyW where
  singManaType = STyW

instance IsManaType 'NonSnow 'TyU where
  singManaType = STyU

instance IsManaType 'NonSnow 'TyB where
  singManaType = STyB

instance IsManaType 'NonSnow 'TyR where
  singManaType = STyR

instance IsManaType 'NonSnow 'TyG where
  singManaType = STyG

instance IsManaType 'NonSnow 'TyC where
  singManaType = STyC

instance IsManaType 'NonSnow 'Ty1 where
  singManaType = STy1

instance IsManaType 'Snow 'TyS where
  singManaType = STyS

instance IsManaType 'NonSnow 'TyBG where
  singManaType = STyBG

instance IsManaType 'NonSnow 'TyPW where
  singManaType = STyPW

instance IsManaType 'NonSnow 'TyPU where
  singManaType = STyPU

instance IsManaType 'NonSnow 'TyPB where
  singManaType = STyPB

instance IsManaType 'NonSnow 'TyPR where
  singManaType = STyPR

instance IsManaType 'NonSnow 'TyPG where
  singManaType = STyPG

instance IsManaType 'NonSnow 'TyPC where
  singManaType = STyPC
