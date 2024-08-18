{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Zone (
  Zone (..),
  IsZone (..),
  SingZone (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)

data Zone :: Type where
  ZBattlefield :: Zone
  ZExile :: Zone
  ZGraveyard :: Zone
  ZHand :: Zone
  ZLibrary :: Zone
  ZStack :: Zone
  deriving (Eq, Ord, Show, Typeable)

data SingZone (zone :: Zone) :: Type where
  SingZBattlefield :: SingZone 'ZBattlefield
  SingZExile :: SingZone 'ZExile
  SingZGraveyard :: SingZone 'ZGraveyard
  SingZHand :: SingZone 'ZHand
  SingZLibrary :: SingZone 'ZLibrary
  SingZStack :: SingZone 'ZStack
  deriving (Typeable)

deriving instance Eq (SingZone zone)

deriving instance Ord (SingZone zone)

deriving instance Show (SingZone zone)

class (Typeable zone) => IsZone zone where
  singZone :: SingZone zone
  litZone :: Zone

instance IsZone 'ZBattlefield where
  singZone :: SingZone 'ZBattlefield
  singZone = SingZBattlefield

  litZone :: Zone
  litZone = ZBattlefield

instance IsZone 'ZExile where
  singZone :: SingZone 'ZExile
  singZone = SingZExile

  litZone :: Zone
  litZone = ZExile

instance IsZone 'ZGraveyard where
  singZone :: SingZone 'ZGraveyard
  singZone = SingZGraveyard

  litZone :: Zone
  litZone = ZGraveyard

instance IsZone 'ZHand where
  singZone :: SingZone 'ZHand
  singZone = SingZHand

  litZone :: Zone
  litZone = ZHand

instance IsZone 'ZLibrary where
  singZone :: SingZone 'ZLibrary
  singZone = SingZLibrary

  litZone :: Zone
  litZone = ZLibrary

instance IsZone 'ZStack where
  singZone :: SingZone 'ZStack
  singZone = SingZStack

  litZone :: Zone
  litZone = ZStack
