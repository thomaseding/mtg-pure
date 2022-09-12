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

import safe Data.Inst (Inst1, Inst2, Inst3, Inst4, Inst5)
import safe Data.Kind (Type)
import safe MtgPure.Model.BasicLandType (BasicLandType)
import safe MtgPure.Model.CardName (CardName)
import safe MtgPure.Model.CardSet (CardSet)
import safe MtgPure.Model.Colors (Colors)
import safe MtgPure.Model.CreatureType (CreatureType)
import safe MtgPure.Model.Damage (Damage)
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.IsObjectType (IsObjectType)
import safe MtgPure.Model.Loyalty (Loyalty)
import safe MtgPure.Model.ManaCost (ManaCost)
import safe MtgPure.Model.ManaPool (ManaPool)
import safe MtgPure.Model.ObjectN (ObjectN)
import safe MtgPure.Model.ObjectN.Type
  ( OActivatedOrTriggeredAbility,
    OAny,
    OCreaturePlayerPlaneswalker,
    ODamageSource,
    OPermanent,
    OPlayer,
    OSpell,
  )
import safe MtgPure.Model.ObjectType.Any (WAny)
import safe MtgPure.Model.ObjectType.Kind
  ( OTArtifact,
    OTArtifactCreature,
    OTCard,
    OTCreature,
    OTEnchantment,
    OTInstant,
    OTLand,
    OTPlaneswalker,
    OTPlayer,
    OTSorcery,
  )
import safe MtgPure.Model.ObjectType.NonCreatureCard (WNonCreatureCard)
import safe MtgPure.Model.ObjectType.Permanent (WPermanent)
import safe MtgPure.Model.Power (Power)
import safe MtgPure.Model.Rarity (Rarity)
import safe MtgPure.Model.Selection (Selection)
import safe MtgPure.Model.TimePoint (TimePoint)
import safe MtgPure.Model.Toughness (Toughness)
import safe MtgPure.Model.Tribal (Tribal (..))
import safe MtgPure.Model.Variable (Variable)

data Ability :: forall a. a -> Type where
  Activated :: Elect Cost a -> Elect 'OneShot a -> Ability a
  Static :: StaticAbility a -> Ability a
  Triggered :: TriggeredAbility a -> Ability a

data Card :: forall a. a -> Type where
  ArtifactCard :: Card OTArtifact -> Card OTCard
  Card1 :: IsObjectType a => CardName -> (ObjectN a {-this-} -> CardTypeDef t a) -> Card a
  Card2 :: Inst2 IsObjectType a b => CardName -> (ObjectN '(a, b {-this-}) -> CardTypeDef t '(a, b)) -> Card '(a, b)
  CreatureCard :: Card OTCreature -> Card OTCard
  EnchantmentCard :: Card OTEnchantment -> Card OTCard
  InstantCard :: Card OTInstant -> Card OTCard
  LandCard :: Card OTLand -> Card OTCard
  PlaneswalkerCard :: Card OTPlaneswalker -> Card OTCard
  SorceryCard :: Card OTSorcery -> Card OTCard

-- TODO: Add encoding for compositing distinct permanent card types. `this` still needs to work
-- Can composite: Artifact/Creature/Enchantment only? Lands dont have costs, and planeswalkers are special.
data CardTypeDef :: forall a. Tribal -> a -> Type where
  ArtifactCreatureDef :: Colors -> Elect Cost OTArtifactCreature -> [CreatureType] -> Power -> Toughness -> [Ability OTArtifactCreature] -> CardTypeDef t OTArtifactCreature
  ArtifactDef :: Colors -> Elect Cost OTArtifact -> [Ability OTArtifact] -> CardTypeDef t OTArtifact
  CreatureDef :: Colors -> Elect Cost OTCreature -> [CreatureType] -> Power -> Toughness -> [Ability OTCreature] -> CardTypeDef t OTCreature
  EnchantmentDef :: Colors -> Elect Cost OTEnchantment -> [Ability OTEnchantment] -> CardTypeDef t OTEnchantment
  InstantDef :: Colors -> Elect Cost OTInstant -> [Ability OTInstant] -> Elect 'OneShot OTInstant -> CardTypeDef t OTInstant
  LandDef :: [Ability OTLand] -> CardTypeDef t OTLand
  PlaneswalkerDef :: Colors -> Elect Cost OTPlaneswalker -> Loyalty -> [Ability OTPlaneswalker] -> CardTypeDef t OTPlaneswalker
  SorceryDef :: Colors -> Elect Cost OTSorcery -> [Ability OTSorcery] -> Elect 'OneShot OTSorcery -> CardTypeDef t OTSorcery
  TribalDef :: [CreatureType] -> WNonCreatureCard a -> CardTypeDef 'NonTribal a -> CardTypeDef 'Tribal a
  VariableDef :: (Variable -> CardTypeDef t a) -> CardTypeDef t a

data Condition :: Type where
  CAnd :: [Condition] -> Condition
  COr :: [Condition] -> Condition
  Satisfies :: WAny a -> ObjectN a -> [Requirement a] -> Condition

data Cost :: forall a. a -> Type where
  AndCosts :: [Cost a] -> Cost a
  DiscardRandomCost :: Int -> Cost a -- TODO: PositiveInt
  LoyaltyCost :: Loyalty -> Cost OTPlaneswalker
  ManaCost :: ManaCost -> Cost a
  OrCosts :: [Cost a] -> Cost a
  PayLife :: Int -> Cost a -- TODO: PositiveInt
  SacrificeCost :: WPermanent a -> [Requirement a] -> Cost a
  TapCost :: OPermanent -> Cost a

data Effect :: EffectType -> Type where
  AddMana :: ManaPool -> OPlayer -> Effect 'OneShot
  AddToBattlefield :: WPermanent a -> OPlayer -> Token a -> Effect 'OneShot
  ChangeTo :: WPermanent a -> OPermanent -> Card a -> Effect 'Continuous
  CounterAbility :: OActivatedOrTriggeredAbility -> Effect 'OneShot
  CounterSpell :: OSpell -> Effect 'OneShot
  DealDamage :: ODamageSource -> OCreaturePlayerPlaneswalker -> Damage -> Effect 'OneShot
  Destroy :: OPermanent -> Effect 'OneShot
  DrawCards :: OPlayer -> Int -> Effect 'OneShot
  Sacrifice :: WPermanent a -> OPlayer -> [Requirement a] -> Effect 'OneShot

data Elect :: forall e a. e -> a -> Type where
  A :: Selection -> WithObject (Elect e) a -> Elect e a
  ActivePlayer :: (OPlayer -> Elect e a) -> Elect e a
  All :: WithObject (Elect e) a -> Elect e a
  Condition :: Condition -> Elect Condition a
  ControllerOf :: OAny -> (OPlayer -> Elect e a) -> Elect e a
  Cost :: Cost a -> Elect Cost a
  Effect :: [Effect e] -> Elect e a
  Event :: EventListener a -> Elect EventListener a
  If :: Condition -> Elect e a -> Elect e a -> Elect e a

data EventListener :: forall a. a -> Type where
  Events :: [EventListener a] -> EventListener a
  SpellIsCast :: WithObject (Elect 'OneShot) a -> EventListener a
  TimePoint :: TimePoint p -> Elect 'OneShot a -> EventListener a

data Requirement :: forall a. a -> Type where
  ControlledBy :: OPlayer -> Requirement a
  HasAbility :: Ability a -> Requirement a -- Non-unique differing representations will not be considered the same
  HasBasicLandType :: BasicLandType -> Requirement OTLand
  Impossible :: Requirement a
  Is :: WAny a -> ObjectN a -> Requirement a
  Not :: Requirement a -> Requirement a
  OfColors :: Colors -> Requirement a -- needs `WCard a` witness
  OwnedBy :: OPlayer -> Requirement a
  PlayerPays :: Cost OTPlayer -> Requirement OTPlayer
  RAnd :: [Requirement a] -> Requirement a
  ROr :: [Requirement a] -> Requirement a
  Tapped :: WPermanent a -> Requirement a
  -- TODO: Try to add some combinators that go from: forall a b. [forall x. Requirement x] -> Requirement '(a, b)
  R2 :: Inst2 IsObjectType a b => [Requirement a] -> [Requirement b] -> Requirement '(a, b)
  R3 :: Inst3 IsObjectType a b c => [Requirement a] -> [Requirement b] -> [Requirement c] -> Requirement '(a, b, c)
  R4 :: Inst4 IsObjectType a b c d => [Requirement a] -> [Requirement b] -> [Requirement c] -> [Requirement d] -> Requirement '(a, b, c, d)
  R5 :: Inst5 IsObjectType a b c d e => [Requirement a] -> [Requirement b] -> [Requirement c] -> [Requirement d] -> [Requirement e] -> Requirement '(a, b, c, d, e)

data SetCard :: forall a. a -> Type where
  SetCard :: CardSet -> Rarity -> Card a -> SetCard a

data SetToken :: forall a. a -> Type where
  SetToken :: CardSet -> Rarity -> Token a -> SetToken a

data StaticAbility :: forall a. a -> Type where
  As :: WithObject EventListener a -> StaticAbility a -- 603.6d: not a triggered ability
  ContinuousEffect :: Elect 'Continuous a -> StaticAbility a
  FirstStrike :: StaticAbility OTCreature
  Flying :: StaticAbility OTCreature
  Haste :: StaticAbility OTCreature
  Suspend :: Int -> Elect Cost a -> StaticAbility a

data Token :: forall a. a -> Type where
  Token :: Card a -> Token a

-- https://www.mtgsalvation.com/forums/magic-fundamentals/magic-rulings/magic-rulings-archives/611601-whenever-what-does-it-mean?comment=3
-- https://www.reddit.com/r/magicTCG/comments/asmecb/noob_question_difference_between_as_and_when/
data TriggeredAbility :: forall a. a -> Type where
  When :: Elect EventListener a -> TriggeredAbility a

data WithObject :: forall o x. x -> o -> Type where
  O1 :: Inst1 IsObjectType a => [Requirement a] -> (ObjectN a -> x o) -> WithObject x o
  O2 :: Inst2 IsObjectType a b => [Requirement '(a, b)] -> (ObjectN '(a, b) -> x o) -> WithObject x o
  O3 :: Inst3 IsObjectType a b c => [Requirement '(a, b, c)] -> (ObjectN '(a, b, c) -> x o) -> WithObject x o
  O4 :: Inst4 IsObjectType a b c d => [Requirement '(a, b, c, d)] -> (ObjectN '(a, b, c, d) -> x o) -> WithObject x o
  O5 :: Inst5 IsObjectType a b c d e => [Requirement '(a, b, c, d, e)] -> (ObjectN '(a, b, c, d, e) -> x o) -> WithObject x o
