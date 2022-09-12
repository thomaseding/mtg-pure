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
import safe Data.Typeable (Typeable)
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
import MtgPure.Model.ObjectType (OT)
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
import safe MtgPure.Model.VisitObjectN (VisitObjectN)

type TypeableOT k ot = (Typeable k, Typeable (ot :: k), VisitObjectN ot)

type TypeableOT2 k ot x = (Typeable k, Typeable (ot :: k), Typeable (x :: k -> Type), VisitObjectN ot)

data Ability :: forall ot. ot -> Type where
  Activated :: Elect (Cost ot) ot -> Elect (Effect 'OneShot) ot -> Ability ot
  Static :: StaticAbility ot -> Ability ot
  Triggered :: TriggeredAbility ot -> Ability ot
  deriving (Typeable)

data Card :: forall ot. ot -> Type where
  ArtifactCard :: Card OTArtifact -> Card OTCard
  Card1 :: IsObjectType a => CardName -> (ObjectN '(OT, a {-this-}) -> CardTypeDef t '(OT, a)) -> Card '(OT, a)
  Card2 :: Inst2 IsObjectType a b => CardName -> (ObjectN '(OT, a, b {-this-}) -> CardTypeDef t '(OT, a, b)) -> Card '(OT, a, b)
  CreatureCard :: Card OTCreature -> Card OTCard
  EnchantmentCard :: Card OTEnchantment -> Card OTCard
  InstantCard :: Card OTInstant -> Card OTCard
  LandCard :: Card OTLand -> Card OTCard
  PlaneswalkerCard :: Card OTPlaneswalker -> Card OTCard
  SorceryCard :: Card OTSorcery -> Card OTCard
  deriving (Typeable)

data CardTypeDef :: forall ot. Tribal -> ot -> Type where
  ArtifactCreatureDef ::
    Colors ->
    Elect (Cost OTArtifactCreature) OTArtifactCreature ->
    [CreatureType] ->
    Power ->
    Toughness ->
    [Ability OTArtifactCreature] ->
    CardTypeDef t OTArtifactCreature
  ArtifactDef ::
    Colors ->
    Elect (Cost OTArtifact) OTArtifact ->
    [Ability OTArtifact] ->
    CardTypeDef t OTArtifact
  CreatureDef ::
    Colors ->
    Elect (Cost OTCreature) OTCreature ->
    [CreatureType] ->
    Power ->
    Toughness ->
    [Ability OTCreature] ->
    CardTypeDef t OTCreature
  EnchantmentDef ::
    Colors ->
    Elect (Cost OTEnchantment) OTEnchantment ->
    [Ability OTEnchantment] ->
    CardTypeDef t OTEnchantment
  InstantDef ::
    Colors ->
    Elect (Cost OTInstant) OTInstant ->
    [Ability OTInstant] ->
    Elect (Effect 'OneShot) OTInstant ->
    CardTypeDef t OTInstant
  LandDef ::
    [Ability OTLand] ->
    CardTypeDef t OTLand
  PlaneswalkerDef ::
    Colors ->
    Elect (Cost OTPlaneswalker) OTPlaneswalker ->
    Loyalty ->
    [Ability OTPlaneswalker] ->
    CardTypeDef t OTPlaneswalker
  SorceryDef ::
    Colors ->
    Elect (Cost OTSorcery) OTSorcery ->
    [Ability OTSorcery] ->
    Elect (Effect 'OneShot) OTSorcery ->
    CardTypeDef t OTSorcery
  TribalDef ::
    [CreatureType] ->
    WNonCreatureCard ot ->
    CardTypeDef 'NonTribal ot ->
    CardTypeDef 'Tribal ot
  VariableDef ::
    (Variable -> CardTypeDef t ot) ->
    CardTypeDef t ot
  deriving (Typeable)

data Condition :: Type where
  CAnd :: [Condition] -> Condition
  COr :: [Condition] -> Condition
  Satisfies :: TypeableOT k ot => WAny ot -> ObjectN ot -> [Requirement ot] -> Condition
  deriving (Typeable)

data Cost :: forall ot. ot -> Type where
  AndCosts :: [Cost ot] -> Cost ot
  DiscardRandomCost :: Int -> Cost ot -- TODO: PositiveInt
  LoyaltyCost :: Loyalty -> Cost OTPlaneswalker
  ManaCost :: ManaCost -> Cost ot
  OrCosts :: [Cost ot] -> Cost ot
  PayLife :: Int -> Cost ot -- TODO: PositiveInt
  SacrificeCost :: TypeableOT k ot => WPermanent ot -> [Requirement ot] -> Cost ot
  TapCost :: OPermanent -> Cost ot
  deriving (Typeable)

data Effect :: EffectType -> Type where
  AddMana :: ManaPool -> OPlayer -> Effect 'OneShot
  AddToBattlefield :: TypeableOT k ot => WPermanent ot -> OPlayer -> Token ot -> Effect 'OneShot
  ChangeTo :: TypeableOT k ot => WPermanent ot -> OPermanent -> Card ot -> Effect 'Continuous
  CounterAbility :: OActivatedOrTriggeredAbility -> Effect 'OneShot
  CounterSpell :: OSpell -> Effect 'OneShot
  DealDamage :: ODamageSource -> OCreaturePlayerPlaneswalker -> Damage -> Effect 'OneShot
  Destroy :: OPermanent -> Effect 'OneShot
  DrawCards :: OPlayer -> Int -> Effect 'OneShot
  Sacrifice :: TypeableOT k ot => WPermanent ot -> OPlayer -> [Requirement ot] -> Effect 'OneShot
  deriving (Typeable)

data Elect :: forall ot. Type -> ot -> Type where
  A :: TypeableOT2 k ot (Elect e) => Selection -> WithObject (Elect e) ot -> Elect e ot
  ActivePlayer :: (OPlayer -> Elect e ot) -> Elect e ot
  All :: WithObject (Elect e) ot -> Elect e ot
  Condition :: Condition -> Elect Condition ot
  ControllerOf :: OAny -> (OPlayer -> Elect e ot) -> Elect e ot
  Cost :: Cost ot -> Elect (Cost ot) ot
  Effect :: [Effect e] -> Elect (Effect e) ot
  Event :: EventListener ot -> Elect (EventListener ot) ot
  If :: Condition -> Elect e ot -> Elect e ot -> Elect e ot
  deriving (Typeable)

data EventListener :: forall ot. ot -> Type where
  Events :: [EventListener ot] -> EventListener ot
  SpellIsCast :: WithObject (Elect (Effect 'OneShot)) ot -> EventListener ot
  TimePoint :: Typeable p => TimePoint p -> Elect (Effect 'OneShot) ot -> EventListener ot
  deriving (Typeable)

data Requirement :: forall ot. ot -> Type where
  ControlledBy :: OPlayer -> Requirement ot
  HasAbility :: Ability ot -> Requirement ot -- Non-unique differing representations will not be considered the same
  HasBasicLandType :: BasicLandType -> Requirement OTLand
  Impossible :: Requirement ot
  Is :: TypeableOT k ot => WAny ot -> ObjectN ot -> Requirement ot
  Not :: TypeableOT k ot => Requirement ot -> Requirement ot
  OfColors :: Colors -> Requirement ot -- needs `WCard a` witness
  OwnedBy :: OPlayer -> Requirement ot
  PlayerPays :: Cost OTPlayer -> Requirement OTPlayer
  RAnd :: [Requirement ot] -> Requirement ot
  ROr :: [Requirement ot] -> Requirement ot
  Tapped :: TypeableOT k ot => WPermanent ot -> Requirement ot
  -- TODO: Try to add some combinators that go from: forall a b. [forall x. Requirement x] -> Requirement '(OT, a, b)
  R2 :: Inst2 IsObjectType a b => [Requirement '(OT, a)] -> [Requirement '(OT, b)] -> Requirement '(OT, a, b)
  R3 :: Inst3 IsObjectType a b c => [Requirement '(OT, a)] -> [Requirement '(OT, b)] -> [Requirement '(OT, c)] -> Requirement '(OT, a, b, c)
  R4 :: Inst4 IsObjectType a b c d => [Requirement '(OT, a)] -> [Requirement '(OT, b)] -> [Requirement '(OT, c)] -> [Requirement '(OT, d)] -> Requirement '(OT, a, b, c, d)
  R5 :: Inst5 IsObjectType a b c d e => [Requirement '(OT, a)] -> [Requirement '(OT, b)] -> [Requirement '(OT, c)] -> [Requirement '(OT, d)] -> [Requirement '(OT, e)] -> Requirement '(OT, a, b, c, d, e)
  deriving (Typeable)

data SetCard :: forall ot. ot -> Type where
  SetCard :: CardSet -> Rarity -> Card ot -> SetCard ot
  deriving (Typeable)

data SetToken :: forall ot. ot -> Type where
  SetToken :: CardSet -> Rarity -> Token ot -> SetToken ot
  deriving (Typeable)

data StaticAbility :: forall ot. ot -> Type where
  As :: TypeableOT2 k ot EventListener => WithObject EventListener ot -> StaticAbility ot -- 603.6d: not a triggered ability
  ContinuousEffect :: Elect (Effect 'Continuous) ot -> StaticAbility ot
  FirstStrike :: StaticAbility OTCreature
  Flying :: StaticAbility OTCreature
  Haste :: StaticAbility OTCreature
  Suspend :: Int -> Elect (Cost ot) ot -> StaticAbility ot
  deriving (Typeable)

data Token :: forall ot. ot -> Type where
  Token :: Card ot -> Token ot
  deriving (Typeable)

-- https://www.mtgsalvation.com/forums/magic-fundamentals/magic-rulings/magic-rulings-archives/611601-whenever-what-does-it-mean?comment=3
-- https://www.reddit.com/r/magicTCG/comments/asmecb/noob_question_difference_between_as_and_when/
data TriggeredAbility :: forall ot. ot -> Type where
  When :: TypeableOT k ot => Elect (EventListener ot) ot -> TriggeredAbility ot
  deriving (Typeable)

data WithObject :: forall ot x. x -> ot -> Type where
  O1 :: (TypeableOT2 k ot x, Inst1 IsObjectType a) => [Requirement '(OT, a)] -> (ObjectN '(OT, a) -> x ot) -> WithObject x ot
  O2 :: (TypeableOT2 k ot x, Inst2 IsObjectType a b) => [Requirement '(OT, a, b)] -> (ObjectN '(OT, a, b) -> x ot) -> WithObject x ot
  O3 :: (TypeableOT2 k ot x, Inst3 IsObjectType a b c) => [Requirement '(OT, a, b, c)] -> (ObjectN '(OT, a, b, c) -> x ot) -> WithObject x ot
  O4 :: (TypeableOT2 k ot x, Inst4 IsObjectType a b c d) => [Requirement '(OT, a, b, c, d)] -> (ObjectN '(OT, a, b, c, d) -> x ot) -> WithObject x ot
  O5 :: (TypeableOT2 k ot x, Inst5 IsObjectType a b c d e) => [Requirement '(OT, a, b, c, d, e)] -> (ObjectN '(OT, a, b, c, d, e) -> x ot) -> WithObject x ot
  deriving (Typeable)
