{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ObjectType (
  ObjectType (..),
  SObjectType (..),
  OTArbitrary,
  pattern OTArbitrary,
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)

-- This generalizes the notion of "object" in MTG (e.g. contains "player")
--
-- Don't bother encoding OTManaAbility; since mana abilities are activated/triggered abilities and cannot be interacted with (605.3b)
--
-- If combinatorial explosion starts to be an issue, just stop the ToObjectN limit to that of 12 or so and also special case those
-- types to directly jump to ToObjectAny. Then it's the client's responsibility to provide any obtuse instances for ToObjectN.
data ObjectType
  = OTActivatedAbility
  | OTArtifact
  | OTCreature
  | OTEmblem
  | OTEnchantment
  | OTInstant
  | OTLand
  | OTPlaneswalker
  | OTPlayer
  | OTSorcery
  | OTStaticAbility
  | OTTriggeredAbility
  deriving (Bounded, Enum, Eq, Ord, Show, Typeable)

type OTArbitrary = 'OTLand

pattern OTArbitrary :: ObjectType
pattern OTArbitrary = OTLand

data SObjectType :: ObjectType -> Type where
  SActivatedAbility :: SObjectType 'OTActivatedAbility
  SArtifact :: SObjectType 'OTArtifact
  SCreature :: SObjectType 'OTCreature
  SEmblem :: SObjectType 'OTEmblem
  SEnchantment :: SObjectType 'OTEnchantment
  SInstant :: SObjectType 'OTInstant
  SLand :: SObjectType 'OTLand
  SPlaneswalker :: SObjectType 'OTPlaneswalker
  SPlayer :: SObjectType 'OTPlayer
  SSorcery :: SObjectType 'OTSorcery
  SStaticAbility :: SObjectType 'OTStaticAbility
  STriggeredAbility :: SObjectType 'OTTriggeredAbility
  deriving (Typeable)

deriving instance Eq (SObjectType a)

deriving instance Ord (SObjectType a)

deriving instance Show (SObjectType a)
