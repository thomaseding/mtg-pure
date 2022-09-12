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
    OTArtifact,
    OTCreature,
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
    OTNonLandPermanent,
    OTNonPlaneswalkerPermanent,
    OTPermanent,
    OTCard,
    OTAny,
  )
where

import Data.Kind (Type)
import Data.Typeable (Typeable)

data ObjectType
  = OTArtifact
  | OTCreature
  | OTEnchantment
  | OTInstant
  | OTLand
  | OTPlaneswalker
  | OTPlayer
  | OTSorcery
  deriving (Bounded, Enum, Eq, Ord, Show)

-- XXX: Data.Sing
data SObjectType :: ObjectType -> Type where
  SArtifact :: SObjectType OTArtifact
  SCreature :: SObjectType OTCreature
  SEnchantment :: SObjectType OTEnchantment
  SInstant :: SObjectType OTInstant
  SLand :: SObjectType OTLand
  SPlaneswalker :: SObjectType OTPlaneswalker
  SPlayer :: SObjectType OTPlayer
  SSorcery :: SObjectType OTSorcery
  deriving (Typeable)

deriving instance Show (SObjectType a)

type OTArtifact = 'OTArtifact

type OTCreature = 'OTCreature

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

type OTCard =
  '( OTArtifact,
     OTCreature,
     OTEnchantment,
     OTInstant,
     OTLand,
     OTPlaneswalker,
     OTSorcery
   )

type OTAny =
  '( OTArtifact,
     OTCreature,
     OTEnchantment,
     OTInstant,
     OTLand,
     OTPlaneswalker,
     OTPlayer,
     OTSorcery
   )
