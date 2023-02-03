{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.SObjectType (
  SObjectType (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))

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
