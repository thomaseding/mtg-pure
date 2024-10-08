{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.ManaType (
  ManaType (..),
  ManaTypeToSnow,
  SPoolType (..),
  SCostType (..),
  IsPoolType (..),
  IsCostType (..),
) where

import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Mana.Snow (Snow (..))

data ManaType
  = Ty1
  | TyW
  | TyU
  | TyB
  | TyR
  | TyG
  | TyC
  | TyS
  | TySW
  | TySU
  | TySB
  | TySR
  | TySG
  | TySC
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
  | TyC2
  | TyPW
  | TyPU
  | TyPB
  | TyPR
  | TyPG
  | TyPC
  deriving (Eq, Ord, Typeable)

type family ManaTypeToSnow (mt :: ManaType) = (snow :: Snow) where
  ManaTypeToSnow 'Ty1 = 'NonSnow
  ManaTypeToSnow 'TyW = 'NonSnow
  ManaTypeToSnow 'TyU = 'NonSnow
  ManaTypeToSnow 'TyB = 'NonSnow
  ManaTypeToSnow 'TyR = 'NonSnow
  ManaTypeToSnow 'TyG = 'NonSnow
  ManaTypeToSnow 'TyC = 'NonSnow
  ManaTypeToSnow 'TyS = 'Snow
  ManaTypeToSnow 'TySW = 'Snow
  ManaTypeToSnow 'TySU = 'Snow
  ManaTypeToSnow 'TySB = 'Snow
  ManaTypeToSnow 'TySR = 'Snow
  ManaTypeToSnow 'TySG = 'Snow
  ManaTypeToSnow 'TySC = 'Snow
  ManaTypeToSnow 'TyWU = 'NonSnow
  ManaTypeToSnow 'TyUB = 'NonSnow
  ManaTypeToSnow 'TyBR = 'NonSnow
  ManaTypeToSnow 'TyRG = 'NonSnow
  ManaTypeToSnow 'TyGW = 'NonSnow
  ManaTypeToSnow 'TyWB = 'NonSnow
  ManaTypeToSnow 'TyUR = 'NonSnow
  ManaTypeToSnow 'TyBG = 'NonSnow
  ManaTypeToSnow 'TyRW = 'NonSnow
  ManaTypeToSnow 'TyGU = 'NonSnow
  ManaTypeToSnow 'TyW2 = 'NonSnow
  ManaTypeToSnow 'TyU2 = 'NonSnow
  ManaTypeToSnow 'TyB2 = 'NonSnow
  ManaTypeToSnow 'TyR2 = 'NonSnow
  ManaTypeToSnow 'TyG2 = 'NonSnow
  ManaTypeToSnow 'TyC2 = 'NonSnow
  ManaTypeToSnow 'TyPW = 'NonSnow
  ManaTypeToSnow 'TyPU = 'NonSnow
  ManaTypeToSnow 'TyPB = 'NonSnow
  ManaTypeToSnow 'TyPR = 'NonSnow
  ManaTypeToSnow 'TyPG = 'NonSnow
  ManaTypeToSnow 'TyPC = 'NonSnow

data SPoolType (mt :: ManaType) where
  SPTyW :: SPoolType 'TyW
  SPTyU :: SPoolType 'TyU
  SPTyB :: SPoolType 'TyB
  SPTyR :: SPoolType 'TyR
  SPTyG :: SPoolType 'TyG
  SPTyC :: SPoolType 'TyC
  SPTySW :: SPoolType 'TySW
  SPTySU :: SPoolType 'TySU
  SPTySB :: SPoolType 'TySB
  SPTySR :: SPoolType 'TySR
  SPTySG :: SPoolType 'TySG
  SPTySC :: SPoolType 'TySC

data SCostType (mt :: ManaType) where
  SCTy1 :: SCostType 'Ty1
  SCTyW :: SCostType 'TyW
  SCTyU :: SCostType 'TyU
  SCTyB :: SCostType 'TyB
  SCTyR :: SCostType 'TyR
  SCTyG :: SCostType 'TyG
  SCTyC :: SCostType 'TyC
  SCTyS :: SCostType 'TyS
  SCTyWU :: SCostType 'TyWU
  SCTyUB :: SCostType 'TyUB
  SCTyBR :: SCostType 'TyBR
  SCTyRG :: SCostType 'TyRG
  SCTyGW :: SCostType 'TyGW
  SCTyWB :: SCostType 'TyWB
  SCTyUR :: SCostType 'TyUR
  SCTyBG :: SCostType 'TyBG
  SCTyRW :: SCostType 'TyRW
  SCTyGU :: SCostType 'TyGU
  SCTyW2 :: SCostType 'TyW2
  SCTyU2 :: SCostType 'TyU2
  SCTyB2 :: SCostType 'TyB2
  SCTyR2 :: SCostType 'TyR2
  SCTyG2 :: SCostType 'TyG2
  SCTyC2 :: SCostType 'TyC2
  SCTyPW :: SCostType 'TyPW
  SCTyPU :: SCostType 'TyPU
  SCTyPB :: SCostType 'TyPB
  SCTyPR :: SCostType 'TyPR
  SCTyPG :: SCostType 'TyPG
  SCTyPC :: SCostType 'TyPC

class IsPoolType (mt :: ManaType) where
  singPoolType :: SPoolType mt

instance IsPoolType 'TyW where
  singPoolType :: SPoolType 'TyW
  singPoolType = SPTyW

instance IsPoolType 'TyU where
  singPoolType :: SPoolType 'TyU
  singPoolType = SPTyU

instance IsPoolType 'TyB where
  singPoolType :: SPoolType 'TyB
  singPoolType = SPTyB

instance IsPoolType 'TyR where
  singPoolType :: SPoolType 'TyR
  singPoolType = SPTyR

instance IsPoolType 'TyG where
  singPoolType :: SPoolType 'TyG
  singPoolType = SPTyG

instance IsPoolType 'TyC where
  singPoolType :: SPoolType 'TyC
  singPoolType = SPTyC

instance IsPoolType 'TySW where
  singPoolType :: SPoolType 'TySW
  singPoolType = SPTySW

instance IsPoolType 'TySU where
  singPoolType :: SPoolType 'TySU
  singPoolType = SPTySU

instance IsPoolType 'TySB where
  singPoolType :: SPoolType 'TySB
  singPoolType = SPTySB

instance IsPoolType 'TySR where
  singPoolType :: SPoolType 'TySR
  singPoolType = SPTySR

instance IsPoolType 'TySG where
  singPoolType :: SPoolType 'TySG
  singPoolType = SPTySG

instance IsPoolType 'TySC where
  singPoolType :: SPoolType 'TySC
  singPoolType = SPTySC

class IsCostType (mt :: ManaType) where
  singCostType :: SCostType mt

instance IsCostType 'Ty1 where
  singCostType :: SCostType 'Ty1
  singCostType = SCTy1

instance IsCostType 'TyW where
  singCostType :: SCostType 'TyW
  singCostType = SCTyW

instance IsCostType 'TyU where
  singCostType :: SCostType 'TyU
  singCostType = SCTyU

instance IsCostType 'TyB where
  singCostType :: SCostType 'TyB
  singCostType = SCTyB

instance IsCostType 'TyR where
  singCostType :: SCostType 'TyR
  singCostType = SCTyR

instance IsCostType 'TyG where
  singCostType :: SCostType 'TyG
  singCostType = SCTyG

instance IsCostType 'TyC where
  singCostType :: SCostType 'TyC
  singCostType = SCTyC

instance IsCostType 'TyS where
  singCostType :: SCostType 'TyS
  singCostType = SCTyS

instance IsCostType 'TyWU where
  singCostType :: SCostType 'TyWU
  singCostType = SCTyWU

instance IsCostType 'TyUB where
  singCostType :: SCostType 'TyUB
  singCostType = SCTyUB

instance IsCostType 'TyBR where
  singCostType :: SCostType 'TyBR
  singCostType = SCTyBR

instance IsCostType 'TyRG where
  singCostType :: SCostType 'TyRG
  singCostType = SCTyRG

instance IsCostType 'TyGW where
  singCostType :: SCostType 'TyGW
  singCostType = SCTyGW

instance IsCostType 'TyWB where
  singCostType :: SCostType 'TyWB
  singCostType = SCTyWB

instance IsCostType 'TyUR where
  singCostType :: SCostType 'TyUR
  singCostType = SCTyUR

instance IsCostType 'TyBG where
  singCostType :: SCostType 'TyBG
  singCostType = SCTyBG

instance IsCostType 'TyRW where
  singCostType :: SCostType 'TyRW
  singCostType = SCTyRW

instance IsCostType 'TyGU where
  singCostType :: SCostType 'TyGU
  singCostType = SCTyGU

instance IsCostType 'TyW2 where
  singCostType :: SCostType 'TyW2
  singCostType = SCTyW2

instance IsCostType 'TyU2 where
  singCostType :: SCostType 'TyU2
  singCostType = SCTyU2

instance IsCostType 'TyB2 where
  singCostType :: SCostType 'TyB2
  singCostType = SCTyB2

instance IsCostType 'TyR2 where
  singCostType :: SCostType 'TyR2
  singCostType = SCTyR2

instance IsCostType 'TyG2 where
  singCostType :: SCostType 'TyG2
  singCostType = SCTyG2

instance IsCostType 'TyC2 where
  singCostType :: SCostType 'TyC2
  singCostType = SCTyC2

instance IsCostType 'TyPW where
  singCostType :: SCostType 'TyPW
  singCostType = SCTyPW

instance IsCostType 'TyPU where
  singCostType :: SCostType 'TyPU
  singCostType = SCTyPU

instance IsCostType 'TyPB where
  singCostType :: SCostType 'TyPB
  singCostType = SCTyPB

instance IsCostType 'TyPR where
  singCostType :: SCostType 'TyPR
  singCostType = SCTyPR

instance IsCostType 'TyPG where
  singCostType :: SCostType 'TyPG
  singCostType = SCTyPG

instance IsCostType 'TyPC where
  singCostType :: SCostType 'TyPC
  singCostType = SCTyPC
