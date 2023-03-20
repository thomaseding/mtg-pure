{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.SingOT (
  SingOT (..),
) where

import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object.OT (OT (..))

data SingOT (ot :: OT) where
  SingActivatedAbility :: SingOT 'OTActivatedAbility
  SingArtifact :: SingOT 'OTArtifact
  SingCreature :: SingOT 'OTCreature
  SingEmblem :: SingOT 'OTEmblem
  SingEnchantment :: SingOT 'OTEnchantment
  SingInstant :: SingOT 'OTInstant
  SingLand :: SingOT 'OTLand
  SingPlaneswalker :: SingOT 'OTPlaneswalker
  SingPlayer :: SingOT 'OTPlayer
  SingSorcery :: SingOT 'OTSorcery
  SingStaticAbility :: SingOT 'OTStaticAbility
  SingTriggeredAbility :: SingOT 'OTTriggeredAbility
  deriving (Typeable)

deriving instance Eq (SingOT a)

deriving instance Ord (SingOT a)

deriving instance Show (SingOT a)
