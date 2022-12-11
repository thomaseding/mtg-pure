{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Zone (
  Zone (..),
  SZone (..),
  IsZone (..),
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

data SZone :: Zone -> Type where
  SZBattlefield :: SZone 'ZBattlefield
  SZExile :: SZone 'ZExile
  SZGraveyard :: SZone 'ZGraveyard
  SZHand :: SZone 'ZHand
  SZLibrary :: SZone 'ZLibrary
  SZStack :: SZone 'ZStack
  deriving (Typeable)

deriving instance Eq (SZone zone)

deriving instance Ord (SZone zone)

deriving instance Show (SZone zone)

class Typeable zone => IsZone zone where
  singZone :: SZone zone
  litZone :: Zone

instance IsZone 'ZBattlefield where
  singZone = SZBattlefield
  litZone = ZBattlefield

instance IsZone 'ZExile where
  singZone = SZExile
  litZone = ZExile

instance IsZone 'ZGraveyard where
  singZone = SZGraveyard
  litZone = ZGraveyard

instance IsZone 'ZHand where
  singZone = SZHand
  litZone = ZHand

instance IsZone 'ZLibrary where
  singZone = SZLibrary
  litZone = ZLibrary

instance IsZone 'ZStack where
  singZone = SZStack
  litZone = ZStack
