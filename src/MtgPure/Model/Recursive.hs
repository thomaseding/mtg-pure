{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Recursive where

import safe Data.ConsIndex (ConsIndex (..))
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
    OCreature,
    OCreaturePlayerPlaneswalker,
    ODamageSource,
    OPermanent,
    OPlayer,
    OSpell,
  )
import safe MtgPure.Model.ObjectType (OT)
import safe MtgPure.Model.ObjectType.Any (WAny)
import safe MtgPure.Model.ObjectType.Index (IndexOT)
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

type TypeableOT k ot = (Typeable k, Typeable (ot :: k), VisitObjectN ot, IndexOT ot)

type TypeableOT2 k ot x = (TypeableOT k ot, Typeable (x :: k -> Type))

data Ability :: forall ot. ot -> Type where
  Activated :: Elect (Cost ot) ot -> Elect (Effect 'OneShot) ot -> Ability ot
  Static :: StaticAbility ot -> Ability ot
  Triggered :: TriggeredAbility ot -> Ability ot
  deriving (Typeable)

instance ConsIndex (Ability ot) where
  consIndex = \case
    Activated {} -> 1
    Static {} -> 2
    Triggered {} -> 3

data Card :: forall ot. ot -> Type where
  Card :: CardName -> WithThis (CardTypeDef 'NonTribal) ot -> Card ot
  TribalCard :: CardName -> WithThis (CardTypeDef 'Tribal) ot -> Card ot
  --
  ArtifactCard :: Card OTArtifact -> Card OTCard
  CreatureCard :: Card OTCreature -> Card OTCard
  EnchantmentCard :: Card OTEnchantment -> Card OTCard
  InstantCard :: Card OTInstant -> Card OTCard
  LandCard :: Card OTLand -> Card OTCard
  PlaneswalkerCard :: Card OTPlaneswalker -> Card OTCard
  SorceryCard :: Card OTSorcery -> Card OTCard
  deriving (Typeable)

instance ConsIndex (Card ot) where
  consIndex = \case
    Card {} -> 1
    TribalCard {} -> 2
    ArtifactCard {} -> 3
    CreatureCard {} -> 4
    EnchantmentCard {} -> 5
    InstantCard {} -> 6
    LandCard {} -> 7
    PlaneswalkerCard {} -> 8
    SorceryCard {} -> 9

data CardTypeDef :: forall ot. Tribal -> ot -> Type where
  ArtifactCreatureDef ::
    Colors ->
    Elect (Cost OTArtifactCreature) OTArtifactCreature ->
    [CreatureType] ->
    Power ->
    Toughness ->
    [Ability OTArtifactCreature] ->
    CardTypeDef 'NonTribal OTArtifactCreature
  ArtifactDef ::
    Colors ->
    Elect (Cost OTArtifact) OTArtifact ->
    [Ability OTArtifact] ->
    CardTypeDef 'NonTribal OTArtifact
  CreatureDef ::
    Colors ->
    Elect (Cost OTCreature) OTCreature ->
    [CreatureType] ->
    Power ->
    Toughness ->
    [Ability OTCreature] ->
    CardTypeDef 'NonTribal OTCreature
  EnchantmentDef ::
    Colors ->
    Elect (Cost OTEnchantment) OTEnchantment ->
    [Ability OTEnchantment] ->
    CardTypeDef 'NonTribal OTEnchantment
  InstantDef ::
    Colors ->
    Elect (Cost OTInstant) OTInstant ->
    [Ability OTInstant] ->
    Elect (Effect 'OneShot) OTInstant ->
    CardTypeDef 'NonTribal OTInstant
  LandDef ::
    [Ability OTLand] ->
    CardTypeDef 'NonTribal OTLand
  PlaneswalkerDef ::
    Colors ->
    Elect (Cost OTPlaneswalker) OTPlaneswalker ->
    Loyalty ->
    [Ability OTPlaneswalker] ->
    CardTypeDef 'NonTribal OTPlaneswalker
  SorceryDef ::
    Colors ->
    Elect (Cost OTSorcery) OTSorcery ->
    [Ability OTSorcery] ->
    Elect (Effect 'OneShot) OTSorcery ->
    CardTypeDef 'NonTribal OTSorcery
  TribalDef ::
    [CreatureType] ->
    WNonCreatureCard ot ->
    CardTypeDef 'NonTribal ot ->
    CardTypeDef 'Tribal ot
  VariableDef ::
    (Variable -> CardTypeDef tribal ot) ->
    CardTypeDef tribal ot
  deriving (Typeable)

instance ConsIndex (CardTypeDef tribe ot) where
  consIndex = \case
    ArtifactCreatureDef {} -> 1
    ArtifactDef {} -> 2
    CreatureDef {} -> 3
    EnchantmentDef {} -> 4
    InstantDef {} -> 5
    LandDef {} -> 6
    PlaneswalkerDef {} -> 7
    SorceryDef {} -> 8
    TribalDef {} -> 9
    VariableDef {} -> 10

data Condition :: Type where
  CAnd :: [Condition] -> Condition
  COr :: [Condition] -> Condition
  Satisfies :: TypeableOT k ot => WAny ot -> ObjectN ot -> [Requirement ot] -> Condition
  deriving (Typeable)

instance ConsIndex Condition where
  consIndex = \case
    CAnd {} -> 1
    COr {} -> 2
    Satisfies {} -> 3

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

instance ConsIndex (Cost ot) where
  consIndex = \case
    AndCosts {} -> 1
    DiscardRandomCost {} -> 2
    LoyaltyCost {} -> 3
    ManaCost {} -> 4
    OrCosts {} -> 5
    PayLife {} -> 6
    SacrificeCost {} -> 7
    TapCost {} -> 8

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

instance ConsIndex (Effect e) where
  consIndex = \case
    AddMana {} -> 1
    AddToBattlefield {} -> 2
    ChangeTo {} -> 3
    CounterAbility {} -> 4
    CounterSpell {} -> 5
    DealDamage {} -> 6
    Destroy {} -> 7
    DrawCards {} -> 8
    Sacrifice {} -> 9

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
  VariableFromPower :: OCreature -> (Variable -> Elect e ot) -> Elect e ot
  deriving (Typeable)

instance ConsIndex (Elect e ot) where
  consIndex = \case
    A {} -> 1
    ActivePlayer {} -> 2
    All {} -> 3
    Condition {} -> 4
    ControllerOf {} -> 5
    Cost {} -> 6
    Effect {} -> 7
    Event {} -> 8
    If {} -> 9
    VariableFromPower {} -> 10

data EventListener :: forall ot. ot -> Type where
  Events :: [EventListener ot] -> EventListener ot
  SpellIsCast :: WithObject (Elect (Effect 'OneShot)) ot -> EventListener ot
  TimePoint :: Typeable p => TimePoint p -> Elect (Effect 'OneShot) ot -> EventListener ot
  deriving (Typeable)

instance ConsIndex (EventListener ot) where
  consIndex = \case
    Events {} -> 1
    SpellIsCast {} -> 2
    TimePoint {} -> 3

data Requirement :: forall ot. ot -> Type where
  ControlledBy :: OPlayer -> Requirement ot
  HasAbility :: WithThis Ability ot -> Requirement ot -- Non-unique differing representations will not be considered the same
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

instance ConsIndex (Requirement ot) where
  consIndex = \case
    ControlledBy {} -> 1
    HasAbility {} -> 2
    HasBasicLandType {} -> 3
    Impossible {} -> 4
    Is {} -> 5
    Not {} -> 6
    OfColors {} -> 7
    OwnedBy {} -> 8
    PlayerPays {} -> 9
    RAnd {} -> 10
    ROr {} -> 11
    Tapped {} -> 12
    R2 {} -> 13
    R3 {} -> 14
    R4 {} -> 15
    R5 {} -> 16

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
  Suspend :: Int -> Elect (Cost ot) ot -> StaticAbility ot -- PositiveInt
  deriving (Typeable)

instance ConsIndex (StaticAbility ot) where
  consIndex = \case
    As {} -> 1
    ContinuousEffect {} -> 2
    FirstStrike {} -> 3
    Flying {} -> 4
    Haste {} -> 5
    Suspend {} -> 6

-- TODO: This needs a witness for a token object (`WToken`)
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

instance ConsIndex (WithObject x ot) where
  consIndex = \case
    O1 {} -> 1
    O2 {} -> 2
    O3 {} -> 3
    O4 {} -> 4
    O5 {} -> 5

data WithThis :: forall ot x. x -> ot -> Type where
  T1 :: Inst1 IsObjectType a => (ObjectN '(OT, a) -> x '(OT, a)) -> WithThis x '(OT, a)
  T2 :: Inst2 IsObjectType a b => (ObjectN '(OT, a, b) -> x '(OT, a, b)) -> WithThis x '(OT, a, b)
  T3 :: Inst3 IsObjectType a b c => (ObjectN '(OT, a, b, c) -> x '(OT, a, b, c)) -> WithThis x '(OT, a, b, c)
  T4 :: Inst4 IsObjectType a b c d => (ObjectN '(OT, a, b, c, d) -> x '(OT, a, b, c, d)) -> WithThis x '(OT, a, b, c, d)
  T5 :: Inst5 IsObjectType a b c d e => (ObjectN '(OT, a, b, c, d, e) -> x '(OT, a, b, c, d, e)) -> WithThis x '(OT, a, b, c, d, e)
  deriving (Typeable)

instance ConsIndex (WithThis x ot) where
  consIndex = \case
    T1 {} -> 1
    T2 {} -> 2
    T3 {} -> 3
    T4 {} -> 4
    T5 {} -> 5
