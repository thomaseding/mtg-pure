{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ObjectType
  ( ObjectType (..),
    SObjectType (..),
  )
where

import Data.Kind (Type)
import Data.Typeable (Typeable)

-- This generalizes the notion of "object" in MTG (e.g. contains "player")
-- Don't bother encoding OTManaAbility; since mana abilities are activated/triggered abilities and cannot be interacted with (605.3b)
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
  deriving (Bounded, Enum, Eq, Ord, Show)

-- XXX: Data.Sing
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

deriving instance Show (SObjectType a)
