{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module MtgPure.Model.ObjectType
  ( ObjectType (..),
    SObjectType (..),
    OTAbility,
    OTArtifact,
    OTCreature,
    OTEmblem,
    OTEnchantment,
    OTInstant,
    OTLand,
    OTPlaneswalker,
    OTPlayer,
    OTSorcery,
    OTArtifactCreature,
    OTCreaturePlayer,
    OTCreaturePlaneswalker,
    OTPlayerPlaneswalker,
    OTCreaturePlayerPlaneswalker,
    OTNonArtifactPermanent,
    OTNonCreaturePermanent,
    OTNonEnchantmentPermanent,
    OTDamageSource,
    OTNonLandPermanent,
    OTNonPlaneswalkerPermanent,
    OTPermanent,
    OTNonCreature,
    OTSpell,
    OTCard,
    OTAny,
  )
where

import Data.Kind (Type)
import Data.Typeable (Typeable)

-- This generalizes the notion of "object" in MTG (e.g. contains "player")
data ObjectType
  = OTAbility
  | OTArtifact
  | OTCreature
  | OTEmblem
  | OTEnchantment
  | OTInstant
  | OTLand
  | OTPlaneswalker
  | OTPlayer
  | OTSorcery
  deriving (Bounded, Enum, Eq, Ord, Show)

-- XXX: Data.Sing
data SObjectType :: ObjectType -> Type where
  SAbility :: SObjectType OTAbility
  SArtifact :: SObjectType OTArtifact
  SCreature :: SObjectType OTCreature
  SEmblem :: SObjectType OTEmblem
  SEnchantment :: SObjectType OTEnchantment
  SInstant :: SObjectType OTInstant
  SLand :: SObjectType OTLand
  SPlaneswalker :: SObjectType OTPlaneswalker
  SPlayer :: SObjectType OTPlayer
  SSorcery :: SObjectType OTSorcery
  deriving (Typeable)

deriving instance Show (SObjectType a)

type OTAbility = 'OTAbility

type OTArtifact = 'OTArtifact

type OTCreature = 'OTCreature

type OTEmblem = 'OTEmblem

type OTEnchantment = 'OTEnchantment

type OTInstant = 'OTInstant

type OTLand = 'OTLand

type OTPlaneswalker = 'OTPlaneswalker

type OTPlayer = 'OTPlayer

type OTSorcery = 'OTSorcery

type OTArtifactCreature =
  '( OTArtifact,
     OTCreature
   )

type OTCreaturePlayer =
  '( OTCreature,
     OTPlayer
   )

type OTCreaturePlaneswalker =
  '( OTCreature,
     OTPlaneswalker
   )

type OTPlayerPlaneswalker =
  '( OTPlaneswalker,
     OTPlayer
   )

type OTCreaturePlayerPlaneswalker =
  '( OTCreature,
     OTPlaneswalker,
     OTPlayer
   )

type OTNonArtifactPermanent =
  '( OTCreature,
     OTEnchantment,
     OTLand,
     OTPlaneswalker
   )

type OTNonCreaturePermanent =
  '( OTArtifact,
     OTEnchantment,
     OTLand,
     OTPlaneswalker
   )

type OTNonEnchantmentPermanent =
  '( OTArtifact,
     OTCreature,
     OTLand,
     OTPlaneswalker
   )

type OTNonLandPermanent =
  '( OTArtifact,
     OTCreature,
     OTEnchantment,
     OTPlaneswalker
   )

type OTNonPlaneswalkerPermanent =
  '( OTArtifact,
     OTCreature,
     OTEnchantment,
     OTLand
   )

type OTPermanent =
  '( OTArtifact,
     OTCreature,
     OTEnchantment,
     OTLand,
     OTPlaneswalker
   )

type OTNonCreature =
  '( OTArtifact,
     OTEnchantment,
     OTInstant,
     OTLand,
     OTPlaneswalker,
     OTSorcery
   )

type OTSpell =
  '( OTArtifact,
     OTCreature,
     OTEnchantment,
     OTInstant,
     OTPlaneswalker,
     OTSorcery
   )

type OTCard =
  '( OTArtifact,
     OTCreature,
     OTEnchantment,
     OTInstant,
     OTLand,
     OTPlaneswalker,
     OTSorcery
   )

type OTDamageSource =
  '( OTArtifact,
     OTCreature,
     OTEnchantment,
     OTInstant,
     OTLand,
     OTPlaneswalker,
     OTPlayer,
     OTSorcery
   )

type OTAny =
  '( OTAbility,
     OTArtifact,
     OTCreature,
     OTEmblem,
     OTEnchantment,
     OTInstant,
     OTLand,
     OTPlaneswalker,
     OTPlayer,
     OTSorcery
   )
