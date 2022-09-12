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

import Data.Inst (Inst2, Inst3, Inst4, Inst5)
import Data.Kind (Type)
import MtgPure.Model.CardName (CardName)
import MtgPure.Model.CardSet (CardSet)
import MtgPure.Model.Color (Color)
import MtgPure.Model.Colors (Colors)
import MtgPure.Model.CreatureType (CreatureType)
import MtgPure.Model.Damage (Damage)
import MtgPure.Model.EffectType (EffectType (..))
import MtgPure.Model.IsObjectType (IsObjectType)
import MtgPure.Model.Loyalty (Loyalty)
import MtgPure.Model.ManaCost (ManaCost)
import MtgPure.Model.ManaPool (ManaPool)
import MtgPure.Model.Object (OInstant, OPlaneswalker, OPlayer, OSorcery, Object)
import MtgPure.Model.ObjectN (OAny, OCreaturePlayerPlaneswalker, OPermanent, ObjectN)
import MtgPure.Model.ObjectType
  ( OTArtifact,
    OTCard,
    OTCreature,
    OTEnchantment,
    OTInstant,
    OTLand,
    OTPlaneswalker,
    OTPlayer,
    OTSorcery,
    ObjectType,
  )
import MtgPure.Model.Permanent (Permanent)
import MtgPure.Model.Phase (Phase)
import MtgPure.Model.Power (Power)
import MtgPure.Model.Rarity (Rarity)
import MtgPure.Model.Selection (Selection)
import MtgPure.Model.Step (Step)
import MtgPure.Model.Toughness (Toughness)
import MtgPure.Model.Variable (Variable)

type ActivationCost a = Object a -> OPlayer -> Cost

type SpellCost = OPlayer -> Cost

data Cost :: Type where
  ManaCostCost :: ManaCost -> Cost
  AndCosts :: [Cost] -> Cost
  OrCosts :: [Cost] -> Cost
  TapCost :: OPermanent -> Cost
  LoyaltyCost :: OPlaneswalker -> Loyalty -> Cost
  DiscardRandomCost :: OPlayer -> Int -> Cost
  PayLife :: OPlayer -> Int -> Cost
  SacrificeCost :: Permanent a -> OPlayer -> [Requirement a] -> Cost

data AnyObject :: forall a. a -> Type where
  AnyInstant :: AnyObject OTInstant
  AnySorcery :: AnyObject OTSorcery
  AnyPlayer :: AnyObject OTPlayer
  AnyPermanent :: Permanent a -> AnyObject a

data Requirement :: forall a. a -> Type where
  Impossible :: Requirement a
  OfColors :: Colors -> Requirement a
  Not :: Requirement a -> Requirement a
  Is :: AnyObject a -> ObjectN a -> Requirement a
  ControlledBy :: OPlayer -> Requirement a
  OwnedBy :: OPlayer -> Requirement a
  Tapped :: Permanent a -> Requirement a
  NonBasic :: Requirement OTLand
  PlayerPays :: Cost -> Requirement OTPlayer
  HasTurnControl :: Requirement OTPlayer
  HasBasicLandType :: Color -> Requirement OTLand

data Condition :: Type where
  Or :: [Condition] -> Condition
  And :: [Condition] -> Condition
  Satisfies :: AnyObject a -> ObjectN a -> [Requirement a] -> Condition
  Unless :: Condition -> Condition

data Effect :: EffectType -> Type where
  DoNothing :: Effect e
  AddMana :: ManaPool -> OPlayer -> Effect 'OneShot
  DealDamage :: OAny -> OCreaturePlayerPlaneswalker -> Damage -> Effect 'OneShot
  Sacrifice :: Permanent a -> OPlayer -> [Requirement a] -> Effect 'OneShot
  Destroy :: Permanent a -> ObjectN a -> Effect 'OneShot
  DrawCards :: OPlayer -> Int -> Effect 'OneShot
  ChangeTo :: Permanent a -> OPermanent -> Card a -> Effect 'Continuous

data Elect :: forall e a. e -> a -> Type where
  Condition :: Condition -> Elect Condition a
  ControllerOf :: OAny -> (OPlayer -> Elect e a) -> Elect e a
  A :: Selection -> WithObject (Elect e) a -> Elect e a
  All :: WithObject (Elect e) a -> Elect e a
  Effect :: Effect e -> Elect e a

data WithObject :: forall a x. x -> a -> Type where
  O1 :: IsObjectType b => [Requirement b] -> (Object b -> x a) -> WithObject x a
  O2 :: Inst2 IsObjectType b c => [Requirement b] -> [Requirement c] -> (ObjectN '(b, c) -> x a) -> WithObject x a
  O3 :: Inst3 IsObjectType b c d => [Requirement b] -> [Requirement c] -> [Requirement d] -> (ObjectN '(b, c, d) -> x a) -> WithObject x a
  O4 :: Inst4 IsObjectType b c d e => [Requirement b] -> [Requirement c] -> [Requirement d] -> [Requirement e] -> (ObjectN '(b, c, d, e) -> x a) -> WithObject x a
  O5 :: Inst5 IsObjectType b c d e f => [Requirement b] -> [Requirement c] -> [Requirement d] -> [Requirement e] -> [Requirement f] -> (ObjectN '(b, c, d, e, f) -> x a) -> WithObject x a

data EventListener :: forall a. a -> Type where
  SpellIsCast :: WithObject (Elect 'Continuous) a -> EventListener a

data StaticAbility :: forall a. a -> Type where
  ContinuousEffect :: (Object a -> Elect 'Continuous a) -> StaticAbility a
  Haste :: StaticAbility OTCreature
  FirstStrike :: StaticAbility OTCreature
  Suspend :: Int -> SpellCost -> StaticAbility a
  As :: WithObject EventListener a -> StaticAbility a -- 603.6d: not a triggered ability

-- https://www.mtgsalvation.com/forums/magic-fundamentals/magic-rulings/magic-rulings-archives/611601-whenever-what-does-it-mean?comment=3
-- https://www.reddit.com/r/magicTCG/comments/asmecb/noob_question_difference_between_as_and_when/
data TriggeredAbility :: forall a. a -> Type where
  -- TODO: Needs StartOf EndOf enum argument
  At :: Either Phase (Step p) -> [Object a -> Elect Condition a] -> (Object a -> Elect 'OneShot a) -> TriggeredAbility a
  -- XXX: Are `When` and `Whenever` actually just the same mechanically? If so, consolidate?
  When :: (Object a -> EventListener a) -> TriggeredAbility a
  Whenever :: (Object a -> EventListener a) -> TriggeredAbility a

data Ability :: forall a. a -> Type where
  Activated :: ActivationCost a -> (Object a -> Elect 'OneShot a) -> Ability a
  Static :: StaticAbility a -> Ability a
  Triggered :: TriggeredAbility a -> Ability a

data CardTypeDef :: ObjectType -> Type where
  Variable :: (Variable -> CardTypeDef a) -> CardTypeDef a
  ArtifactDef :: Colors -> SpellCost -> [Ability OTArtifact] -> CardTypeDef OTArtifact
  CreatureDef :: Colors -> SpellCost -> [CreatureType] -> Power -> Toughness -> [Ability OTCreature] -> CardTypeDef OTCreature
  EnchantmentDef :: Colors -> SpellCost -> [Ability OTEnchantment] -> CardTypeDef OTEnchantment
  InstantDef :: Colors -> SpellCost -> [Ability OTInstant] -> (OInstant -> Elect 'OneShot OTInstant) -> CardTypeDef OTInstant
  LandDef :: [Ability OTLand] -> CardTypeDef OTLand
  PlaneswalkerDef :: Colors -> SpellCost -> Loyalty -> [Ability OTPlaneswalker] -> CardTypeDef OTPlaneswalker
  SorceryDef :: Colors -> SpellCost -> [Ability OTSorcery] -> (OSorcery -> Elect 'OneShot OTSorcery) -> CardTypeDef OTSorcery

data Card :: forall a. a -> Type where
  Card :: CardName -> CardSet -> Rarity -> CardTypeDef a -> Card a
  ArtifactCard :: Card OTArtifact -> Card OTCard
  CreatureCard :: Card OTCreature -> Card OTCard
  EnchantmentCard :: Card OTEnchantment -> Card OTCard
  InstantCard :: Card OTInstant -> Card OTCard
  LandCard :: Card OTLand -> Card OTCard
  PlaneswalkerCard :: Card OTPlaneswalker -> Card OTCard
  SorceryCard :: Card OTSorcery -> Card OTCard

data Token :: forall a. a -> Type where
  Token :: Card a -> Token a
