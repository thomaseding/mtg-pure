{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.CardType (
  CardType (..),
  SCardType (..),
  ObjectTypeToCardType,
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object.OT (OT (..))

data CardType :: Type where
  CTArtifact :: CardType
  CTBattle :: CardType
  CTCreature :: CardType
  CTEnchantment :: CardType
  CTInstant :: CardType
  CTLand :: CardType
  CTPlaneswalker :: CardType
  CTSorcery :: CardType
  deriving (Bounded, Enum, Eq, Ord, Show, Typeable)

data SCardType (ct :: CardType) :: Type where
  SCTArtifact :: SCardType 'CTArtifact
  SCTBattle :: SCardType 'CTBattle
  SCTCreature :: SCardType 'CTCreature
  SCTEnchantment :: SCardType 'CTEnchantment
  SCTInstant :: SCardType 'CTInstant
  SCTLand :: SCardType 'CTLand
  SCTPlaneswalker :: SCardType 'CTPlaneswalker
  SCTSorcery :: SCardType 'CTSorcery
  deriving (Typeable)

type family ObjectTypeToCardType (a :: OT) = (ct :: CardType) | ct -> a where
  ObjectTypeToCardType 'OTArtifact = 'CTArtifact
  ObjectTypeToCardType 'OTBattle = 'CTBattle
  ObjectTypeToCardType 'OTCreature = 'CTCreature
  ObjectTypeToCardType 'OTEnchantment = 'CTEnchantment
  ObjectTypeToCardType 'OTInstant = 'CTInstant
  ObjectTypeToCardType 'OTLand = 'CTLand
  ObjectTypeToCardType 'OTPlaneswalker = 'CTPlaneswalker
  ObjectTypeToCardType 'OTSorcery = 'CTSorcery
