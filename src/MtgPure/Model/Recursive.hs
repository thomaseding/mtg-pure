{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Recursive where

import Data.Inst (Inst2, Inst3)
import Data.Kind (Type)
import MtgPure.Model.CardName (CardName)
import MtgPure.Model.CardSet (CardSet)
import MtgPure.Model.CardType (CardType (..))
import MtgPure.Model.Colors (Colors)
import MtgPure.Model.Cost (ActivationCost, SpellCost)
import MtgPure.Model.CreatureType (CreatureType)
import MtgPure.Model.Damage (Damage)
import MtgPure.Model.EffectType (EffectType (..))
import MtgPure.Model.IsObjectType (IsObjectType)
import MtgPure.Model.Loyalty (Loyalty)
import MtgPure.Model.ManaPool (ManaPool)
import MtgPure.Model.Object (OInstant, OPlayer, OSorcery, Object)
import MtgPure.Model.ObjectN (OAny, OCreaturePlayerPlaneswalker, ObjectN)
import MtgPure.Model.ObjectType
  ( OTArtifact,
    OTCreature,
    OTEnchantment,
    OTInstant,
    OTLand,
    OTPlaneswalker,
    OTSorcery,
  )
import MtgPure.Model.Permanent (Permanent)
import MtgPure.Model.Power (Power)
import MtgPure.Model.Rarity (Rarity)
import MtgPure.Model.Requirement (Requirement, RequirementN)
import MtgPure.Model.Selection (Selection)
import MtgPure.Model.Toughness (Toughness)
import MtgPure.Model.Variable (Variable)

data Effect :: EffectType -> Type where
  DoNothing :: Effect e
  AddMana :: ManaPool -> OPlayer -> Effect 'OneShot
  DealDamage :: OAny -> OCreaturePlayerPlaneswalker -> Damage -> Effect 'OneShot
  Sacrifice :: IsObjectType a => Permanent a -> OPlayer -> [Requirement a] -> Effect 'OneShot
  Destroy :: IsObjectType a => Permanent a -> Object a -> Effect 'OneShot
  DrawCards :: OPlayer -> Int -> Effect 'OneShot

data Elect :: forall a. EffectType -> a -> Type where
  ControllerOf :: OAny -> (OPlayer -> Elect e a) -> Elect e a
  A :: A e a -> Elect e a
  All :: All e a -> Elect e a
  Effect :: Effect e -> Elect e a

data A :: EffectType -> a -> Type where
  A1 :: IsObjectType b => Selection -> [Requirement b] -> (Object b -> Elect e a) -> A e a
  A2 :: Inst2 IsObjectType b c => Selection -> RequirementN '(b, c) -> (ObjectN '(b, c) -> Elect e a) -> A e a
  A3 :: Inst3 IsObjectType b c d => Selection -> RequirementN '(b, c, d) -> (ObjectN '(b, c, d) -> Elect e a) -> A e a

data All :: EffectType -> a -> Type where
  All1 :: IsObjectType b => [Requirement b] -> (Object b -> Elect e a) -> All e a
  All2 :: Inst2 IsObjectType b c => RequirementN '(b, c) -> (ObjectN '(b, c) -> Elect e a) -> All e a
  All3 :: Inst3 IsObjectType b c d => RequirementN '(b, c, d) -> (ObjectN '(b, c, d) -> Elect e a) -> All e a

data StaticAbility :: forall a. a -> Type where
  ContinuousEffect :: (Object a -> Elect 'Continuous a) -> StaticAbility a
  Haste :: StaticAbility OTCreature
  FirstStrike :: StaticAbility OTCreature
  Suspend :: Int -> SpellCost -> StaticAbility a

data Ability :: forall a. a -> Type where
  Activated :: ActivationCost a -> (Object a -> Elect 'OneShot a) -> Ability a
  Static :: StaticAbility a -> Ability a

data CardTypeDef :: CardType -> Type where
  Variable :: (Variable -> CardTypeDef a) -> CardTypeDef a
  ArtifactDef :: Colors -> SpellCost -> [Ability OTArtifact] -> CardTypeDef 'CTArtifact
  CreatureDef :: Colors -> SpellCost -> [CreatureType] -> Power -> Toughness -> [Ability OTCreature] -> CardTypeDef 'CTCreature
  EnchantmentDef :: Colors -> SpellCost -> [Ability OTEnchantment] -> CardTypeDef 'CTEnchantment
  InstantDef :: Colors -> SpellCost -> [Ability OTInstant] -> (OInstant -> Elect 'OneShot OTInstant) -> CardTypeDef 'CTInstant
  LandDef :: [Ability OTLand] -> CardTypeDef 'CTLand
  PlaneswalkerDef :: Colors -> SpellCost -> Loyalty -> [Ability OTPlaneswalker] -> CardTypeDef 'CTPlaneswalker
  SorceryDef :: Colors -> SpellCost -> [Ability OTSorcery] -> (OSorcery -> Elect 'OneShot OTSorcery) -> CardTypeDef 'CTSorcery

data Card :: Type where
  Card :: CardName -> CardSet -> Rarity -> CardTypeDef c -> Card

data Token :: Type where
  Token :: Card -> Token
