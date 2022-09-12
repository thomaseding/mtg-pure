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

import Data.Inst (Inst1, Inst2, Inst3, Inst4, Inst5)
import Data.Kind (Type)
import MtgPure.Model.AnyObject (AnyObject)
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
import MtgPure.Model.NonCreature (NonCreature)
import MtgPure.Model.ObjectN
  ( OActivatedOrTriggeredAbility,
    OAny,
    OCreaturePlayerPlaneswalker,
    ODamageSource,
    OPermanent,
    OPlaneswalker,
    OPlayer,
    OSpell,
    ObjectN,
  )
import MtgPure.Model.ObjectType
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
import MtgPure.Model.Permanent (Permanent)
import MtgPure.Model.Power (Power)
import MtgPure.Model.Rarity (Rarity)
import MtgPure.Model.Selection (Selection)
import MtgPure.Model.TimePoint (TimePoint)
import MtgPure.Model.Toughness (Toughness)
import MtgPure.Model.Tribal (Tribal (..))
import MtgPure.Model.Variable (Variable)

data AbilityType :: EffectType -> Type where
  ActivatedAbility :: AbilityType 'OneShot
  ManaAbility :: AbilityType 'OneShot
  StaticAbility :: AbilityType 'Continuous
  TriggeredAbility :: AbilityType 'OneShot

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
  TribalDef :: [CreatureType] -> NonCreature a -> CardTypeDef 'NonTribal a -> CardTypeDef 'Tribal a
  VariableDef :: (Variable -> CardTypeDef t a) -> CardTypeDef t a

data Condition :: Type where
  CAnd :: [Condition] -> Condition
  COr :: [Condition] -> Condition
  Satisfies :: AnyObject a -> ObjectN a -> [Requirement a] -> Condition

data Cost :: Type where
  AndCosts :: [Cost] -> Cost
  DiscardRandomCost :: OPlayer -> Int -> Cost
  LoyaltyCost :: OPlaneswalker -> Loyalty -> Cost
  ManaCostCost :: ManaCost -> Cost
  OrCosts :: [Cost] -> Cost
  PayLife :: OPlayer -> Int -> Cost
  SacrificeCost :: Permanent a -> OPlayer -> [Requirement a] -> Cost
  TapCost :: OPermanent -> Cost

data Effect :: EffectType -> Type where
  AddMana :: ManaPool -> OPlayer -> Effect 'OneShot
  AddToBattlefield :: Permanent a -> OPlayer -> Token a -> Effect 'OneShot
  ChangeTo :: Permanent a -> OPermanent -> Card a -> Effect 'Continuous
  CounterAbility :: OActivatedOrTriggeredAbility -> Effect 'OneShot
  CounterSpell :: OSpell -> Effect 'OneShot
  DealDamage :: ODamageSource -> OCreaturePlayerPlaneswalker -> Damage -> Effect 'OneShot
  Destroy :: OPermanent -> Effect 'OneShot
  DrawCards :: OPlayer -> Int -> Effect 'OneShot
  Sacrifice :: Permanent a -> OPlayer -> [Requirement a] -> Effect 'OneShot

data Elect :: forall e a. e -> a -> Type where
  A :: Selection -> WithObject (Elect e) a -> Elect e a
  ActivePlayer :: (OPlayer -> Elect e a) -> Elect e a
  All :: WithObject (Elect e) a -> Elect e a
  Condition :: Condition -> Elect Condition a
  ControllerOf :: OAny -> (OPlayer -> Elect e a) -> Elect e a
  Cost :: Cost -> Elect Cost a
  Effect :: [Effect e] -> Elect e a
  Event :: EventListener a -> Elect EventListener a
  If :: Condition -> Elect e a -> Elect e a -> Elect e a

data EventListener :: forall a. a -> Type where
  Events :: [EventListener a] -> EventListener a
  SpellIsCast :: WithObject (Elect 'Continuous) a -> EventListener a
  TimePoint :: TimePoint p -> Elect 'OneShot a -> EventListener a

data Requirement :: forall a. a -> Type where
  ControlledBy :: OPlayer -> Requirement a
  HasAbility :: Ability a -> Requirement a -- Non-unique differing representations will not be considered the same
  HasBasicLandType :: Color -> Requirement OTLand
  Impossible :: Requirement a
  Is :: AnyObject a -> ObjectN a -> Requirement a
  Basic :: Requirement OTLand
  Not :: Requirement a -> Requirement a
  OfColors :: Colors -> Requirement a
  OwnedBy :: OPlayer -> Requirement a
  PlayerPays :: Cost -> Requirement OTPlayer
  RAnd :: [Requirement a] -> Requirement a
  ROr :: [Requirement a] -> Requirement a
  Tapped :: Permanent a -> Requirement a
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

fromSetCard :: SetCard a -> Card a
fromSetCard (SetCard _ _ card) = card

fromSetToken :: SetToken a -> Token a
fromSetToken (SetToken _ _ token) = token
