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

module MtgPure.Model.Recursive
  ( TypeableOT,
    TypeableOT2,
    Ability (..),
    Card (..),
    CardTypeDef (..),
    Condition (..),
    Cost (..),
    Effect (..),
    Elect (..),
    Event,
    EventListener,
    EventListener' (..),
    NonProxy (..),
    Requirement (..),
    SetCard (..),
    SetToken (..),
    StaticAbility (..),
    Token (..),
    TriggeredAbility (..),
    WithLinkedObject (..),
    WithMaskedObject (..),
    WithThis (..),
  )
where

import safe Data.ConsIndex (ConsIndex (..))
import safe Data.Inst (Inst1, Inst2, Inst3, Inst4, Inst5)
import safe Data.Kind (Type)
import safe Data.Typeable (Proxy, Typeable)
import safe MtgPure.Model.CardName (CardName)
import safe MtgPure.Model.CardSet (CardSet)
import safe MtgPure.Model.Colors (Colors)
import safe MtgPure.Model.CreatureType (CreatureType)
import safe MtgPure.Model.Damage (Damage)
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.IsObjectType (IsObjectType)
import safe MtgPure.Model.LandType (LandType)
import safe MtgPure.Model.Loyalty (Loyalty)
import safe MtgPure.Model.ManaCost (ManaCost)
import safe MtgPure.Model.ManaPool (ManaPool)
import safe MtgPure.Model.ObjectN.Type
  ( OActivatedOrTriggeredAbility,
    OAny,
    OArtifact,
    OArtifactCreature,
    OCard,
    OCreature,
    OCreaturePlayerPlaneswalker,
    ODamageSource,
    OEnchantment,
    OInstant,
    OLand,
    ON1,
    ON2,
    ON3,
    ON4,
    ON5,
    OPermanent,
    OPlaneswalker,
    OPlayer,
    OSorcery,
    OSpell,
  )
import safe MtgPure.Model.ObjectType.Any (WAny)
import safe MtgPure.Model.ObjectType.Index (IndexOT)
import safe MtgPure.Model.ObjectType.NonCreatureCard (WNonCreatureCard)
import safe MtgPure.Model.ObjectType.Permanent (WPermanent)
import safe MtgPure.Model.ObjectType.Spell (WSpell (..))
import safe MtgPure.Model.Power (Power)
import safe MtgPure.Model.Rarity (Rarity)
import safe MtgPure.Model.Selection (Selection)
import safe MtgPure.Model.TimePoint (TimePoint)
import safe MtgPure.Model.Toughness (Toughness)
import safe MtgPure.Model.Tribal (Tribal (..))
import safe MtgPure.Model.Variable (Variable)
import safe MtgPure.Model.VisitObjectN (VisitObjectN)

type TypeableOT ot = (Typeable ot, VisitObjectN ot, IndexOT ot)

type TypeableOT2 ot x = (TypeableOT ot, Typeable (x :: Type -> Type))

data Ability :: Type -> Type where
  Activated :: Elect (Cost ot) ot -> Elect (Effect 'OneShot) ot -> Ability ot
  Static :: StaticAbility ot -> Ability ot
  Triggered :: TriggeredAbility ot -> Ability ot
  deriving (Typeable)

instance ConsIndex (Ability ot) where
  consIndex = \case
    Activated {} -> 1
    Static {} -> 2
    Triggered {} -> 3

data Card :: Type -> Type where
  Card :: CardName -> WithThis (CardTypeDef 'NonTribal) ot -> Card ot
  TribalCard :: CardName -> WithThis (CardTypeDef 'Tribal) ot -> Card ot
  --
  ArtifactCard :: Card OArtifact -> Card OCard
  CreatureCard :: Card OCreature -> Card OCard
  EnchantmentCard :: Card OEnchantment -> Card OCard
  InstantCard :: Card OInstant -> Card OCard
  LandCard :: Card OLand -> Card OCard
  PlaneswalkerCard :: Card OPlaneswalker -> Card OCard
  SorceryCard :: Card OSorcery -> Card OCard
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

data CardTypeDef :: Tribal -> Type -> Type where
  ArtifactCreatureDef ::
    Colors ->
    Elect (Cost OArtifactCreature) OArtifactCreature ->
    [CreatureType] ->
    Power ->
    Toughness ->
    [Ability OArtifactCreature] ->
    CardTypeDef 'NonTribal OArtifactCreature
  ArtifactDef ::
    Colors ->
    Elect (Cost OArtifact) OArtifact ->
    [Ability OArtifact] ->
    CardTypeDef 'NonTribal OArtifact
  CreatureDef ::
    Colors ->
    Elect (Cost OCreature) OCreature ->
    [CreatureType] ->
    Power ->
    Toughness ->
    [Ability OCreature] ->
    CardTypeDef 'NonTribal OCreature
  EnchantmentDef ::
    Colors ->
    Elect (Cost OEnchantment) OEnchantment ->
    [Ability OEnchantment] ->
    CardTypeDef 'NonTribal OEnchantment
  InstantDef ::
    Colors ->
    Elect (Cost OInstant) OInstant ->
    [Ability OInstant] ->
    Elect (Effect 'OneShot) OInstant ->
    CardTypeDef 'NonTribal OInstant
  LandDef ::
    [LandType] ->
    [Ability OLand] ->
    CardTypeDef 'NonTribal OLand
  PlaneswalkerDef ::
    Colors ->
    Elect (Cost OPlaneswalker) OPlaneswalker ->
    Loyalty ->
    [Ability OPlaneswalker] ->
    CardTypeDef 'NonTribal OPlaneswalker
  SorceryDef ::
    Colors ->
    Elect (Cost OSorcery) OSorcery ->
    [Ability OSorcery] ->
    Elect (Effect 'OneShot) OSorcery ->
    CardTypeDef 'NonTribal OSorcery
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
  Satisfies :: TypeableOT ot => WAny ot -> ot -> [Requirement ot] -> Condition
  deriving (Typeable)

instance ConsIndex Condition where
  consIndex = \case
    CAnd {} -> 1
    COr {} -> 2
    Satisfies {} -> 3

data Cost :: Type -> Type where
  AndCosts :: [Cost ot] -> Cost ot
  DiscardRandomCost :: Int -> Cost ot -- TODO: PositiveInt
  LoyaltyCost :: Loyalty -> Cost OPlaneswalker
  ManaCost :: ManaCost -> Cost ot
  OrCosts :: [Cost ot] -> Cost ot
  PayLife :: Int -> Cost ot -- TODO: PositiveInt
  SacrificeCost :: TypeableOT ot => WPermanent ot -> [Requirement ot] -> Cost ot
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
  AddMana :: OPlayer -> ManaPool -> Effect 'OneShot
  AddToBattlefield :: TypeableOT ot => WPermanent ot -> OPlayer -> Token ot -> Effect 'OneShot
  ChangeTo :: TypeableOT ot => WPermanent ot -> OPermanent -> Card ot -> Effect 'Continuous
  CounterAbility :: OActivatedOrTriggeredAbility -> Effect 'OneShot
  CounterSpell :: OSpell -> Effect 'OneShot
  DealDamage :: ODamageSource -> OCreaturePlayerPlaneswalker -> Damage -> Effect 'OneShot
  Destroy :: OPermanent -> Effect 'OneShot
  DrawCards :: OPlayer -> Int -> Effect 'OneShot
  EffectContinuous :: Effect 'Continuous -> Effect 'OneShot -- 611.2
  EOr :: [Effect e] -> Effect e
  Gain :: TypeableOT ot => WAny ot -> ot -> Ability ot -> Effect 'Continuous
  Lose :: TypeableOT ot => WAny ot -> ot -> Ability ot -> Effect 'Continuous
  Sacrifice :: TypeableOT ot => WPermanent ot -> OPlayer -> [Requirement ot] -> Effect 'OneShot
  StatDelta :: OCreature -> Power -> Toughness -> Effect 'Continuous
  Until :: Elect Event OPlayer -> Effect 'Continuous -> Effect 'Continuous
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
    EffectContinuous {} -> 8
    EOr {} -> 9
    DrawCards {} -> 10
    Gain {} -> 11
    Lose {} -> 12
    Sacrifice {} -> 13
    StatDelta {} -> 14
    Until {} -> 15

data Elect :: Type -> Type -> Type where
  A :: (e ~ Effect 'OneShot, TypeableOT2 ot (Elect e)) => Selection -> OPlayer -> WithMaskedObject (Elect e) ot -> Elect e ot
  ActivePlayer :: (OPlayer -> Elect e ot) -> Elect e ot
  All :: WithMaskedObject (Elect e) ot -> Elect e ot
  Condition :: Condition -> Elect Condition ot
  ControllerOf :: OAny -> (OPlayer -> Elect e ot) -> Elect e ot
  Cost :: Cost ot -> Elect (Cost ot) ot
  Effect :: [Effect e] -> Elect (Effect e) ot
  Event :: Event -> Elect Event ot
  If :: Condition -> Elect e ot -> Elect e ot -> Elect e ot
  Listen :: EventListener -> Elect EventListener ot
  Random :: TypeableOT2 ot (Elect e) => WithMaskedObject (Elect e) ot -> Elect e ot
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
    Listen {} -> 10
    Random {} -> 11
    VariableFromPower {} -> 12

type Event = EventListener' Proxy

type EventListener = EventListener' (Elect (Effect 'OneShot))

data EventListener' :: (Type -> Type) -> Type where
  BecomesTapped :: TypeableOT2 ot x => WPermanent ot -> WithLinkedObject x ot -> EventListener' x
  Events :: [EventListener' x] -> EventListener' x
  SpellIsCast :: TypeableOT ot => WSpell ot -> WithLinkedObject x ot -> EventListener' x
  TimePoint :: Typeable p => TimePoint p -> x OPlayer -> EventListener' x
  deriving (Typeable)

instance ConsIndex (EventListener' x) where
  consIndex = \case
    BecomesTapped {} -> 1
    Events {} -> 2
    SpellIsCast {} -> 3
    TimePoint {} -> 4

data NonProxy :: (Type -> Type) -> Type where
  NonProxyElectEffectOneShot :: NonProxy (Elect (Effect 'OneShot))

instance ConsIndex (NonProxy x) where
  consIndex = \case
    NonProxyElectEffectOneShot -> 1

data Requirement :: Type -> Type where
  ControlledBy :: OPlayer -> Requirement ot
  HasAbility :: WithThis Ability ot -> Requirement ot -- Non-unique differing representations will not be considered the same
  HasLandType :: LandType -> Requirement OLand
  Impossible :: Requirement ot
  Is :: TypeableOT ot => WAny ot -> ot -> Requirement ot
  Not :: TypeableOT ot => Requirement ot -> Requirement ot
  OfColors :: Colors -> Requirement ot -- needs `WCard a` witness
  OwnedBy :: OPlayer -> Requirement ot
  PlayerPays :: Cost OPlayer -> Requirement OPlayer
  RAnd :: [Requirement ot] -> Requirement ot
  ROr :: [Requirement ot] -> Requirement ot
  Tapped :: TypeableOT ot => WPermanent ot -> Requirement ot
  -- TODO: Try to add some combinators that go from: forall a b. [forall x. Requirement x] -> Requirement (ON2 a, b)
  R2 :: Inst2 IsObjectType a b => [Requirement (ON1 a)] -> [Requirement (ON1 b)] -> Requirement (ON2 a b)
  R3 :: Inst3 IsObjectType a b c => [Requirement (ON1 a)] -> [Requirement (ON1 b)] -> [Requirement (ON1 c)] -> Requirement (ON3 a b c)
  R4 :: Inst4 IsObjectType a b c d => [Requirement (ON1 a)] -> [Requirement (ON1 b)] -> [Requirement (ON1 c)] -> [Requirement (ON1 d)] -> Requirement (ON4 a b c d)
  R5 :: Inst5 IsObjectType a b c d e => [Requirement (ON1 a)] -> [Requirement (ON1 b)] -> [Requirement (ON1 c)] -> [Requirement (ON1 d)] -> [Requirement (ON1 e)] -> Requirement (ON5 a b c d e)
  deriving (Typeable)

instance ConsIndex (Requirement ot) where
  consIndex = \case
    ControlledBy {} -> 1
    HasAbility {} -> 2
    HasLandType {} -> 3
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

data SetCard :: Type -> Type where
  SetCard :: CardSet -> Rarity -> Card ot -> SetCard ot
  deriving (Typeable)

instance ConsIndex (SetCard ot) where
  consIndex = \case
    SetCard {} -> 1

data SetToken :: Type -> Type where
  SetToken :: CardSet -> Rarity -> Token ot -> SetToken ot
  deriving (Typeable)

instance ConsIndex (SetToken ot) where
  consIndex = \case
    SetToken {} -> 1

data StaticAbility :: Type -> Type where
  As :: TypeableOT ot => Elect EventListener ot -> StaticAbility ot -- 603.6d: not a triggered ability
  StaticContinuous :: Elect (Effect 'Continuous) ot -> StaticAbility ot -- 611.3
  FirstStrike :: StaticAbility OCreature
  Flying :: StaticAbility OCreature
  Haste :: StaticAbility OCreature
  Suspend :: Int -> Elect (Cost ot) ot -> StaticAbility ot -- PositiveInt
  deriving (Typeable)

instance ConsIndex (StaticAbility ot) where
  consIndex = \case
    As {} -> 1
    StaticContinuous {} -> 2
    FirstStrike {} -> 3
    Flying {} -> 4
    Haste {} -> 5
    Suspend {} -> 6

-- TODO: This needs a witness for a token object (`WToken`)
data Token :: Type -> Type where
  Token :: Card ot -> Token ot
  deriving (Typeable)

instance ConsIndex (Token ot) where
  consIndex = \case
    Token {} -> 1

-- https://www.mtgsalvation.com/forums/magic-fundamentals/magic-rulings/magic-rulings-archives/611601-whenever-what-does-it-mean?comment=3
-- https://www.reddit.com/r/magicTCG/comments/asmecb/noob_question_difference_between_as_and_when/
data TriggeredAbility :: Type -> Type where
  When :: TypeableOT ot => Elect EventListener ot -> TriggeredAbility ot
  deriving (Typeable)

instance ConsIndex (TriggeredAbility ot) where
  consIndex = \case
    When {} -> 1

data WithLinkedObject :: (Type -> Type) -> Type -> Type where
  LProxy :: [Requirement ot] -> WithLinkedObject Proxy ot
  L1 :: Inst1 IsObjectType a => NonProxy x -> [Requirement (ON1 a)] -> (ON1 a -> x (ON1 a)) -> WithLinkedObject x (ON1 a)
  L2 :: Inst2 IsObjectType a b => NonProxy x -> [Requirement (ON2 a b)] -> (ON2 a b -> x (ON2 a b)) -> WithLinkedObject x (ON2 a b)
  L3 :: Inst3 IsObjectType a b c => NonProxy x -> [Requirement (ON3 a b c)] -> (ON3 a b c -> x (ON3 a b c)) -> WithLinkedObject x (ON3 a b c)
  L4 :: Inst4 IsObjectType a b c d => NonProxy x -> [Requirement (ON4 a b c d)] -> (ON4 a b c d -> x (ON4 a b c d)) -> WithLinkedObject x (ON4 a b c d)
  L5 :: Inst5 IsObjectType a b c d e => NonProxy x -> [Requirement (ON5 a b c d e)] -> (ON5 a b c d e -> x (ON5 a b c d e)) -> WithLinkedObject x (ON5 a b c d e)
  deriving (Typeable)

instance ConsIndex (WithLinkedObject x ot) where
  consIndex = \case
    LProxy {} -> 1
    L1 {} -> 2
    L2 {} -> 3
    L3 {} -> 4
    L4 {} -> 5
    L5 {} -> 6

data WithMaskedObject :: (Type -> Type) -> Type -> Type where
  M1 :: (TypeableOT2 ot x, Inst1 IsObjectType a) => [Requirement (ON1 a)] -> (ON1 a -> x ot) -> WithMaskedObject x ot
  M2 :: (TypeableOT2 ot x, Inst2 IsObjectType a b) => [Requirement (ON2 a b)] -> (ON2 a b -> x ot) -> WithMaskedObject x ot
  M3 :: (TypeableOT2 ot x, Inst3 IsObjectType a b c) => [Requirement (ON3 a b c)] -> (ON3 a b c -> x ot) -> WithMaskedObject x ot
  M4 :: (TypeableOT2 ot x, Inst4 IsObjectType a b c d) => [Requirement (ON4 a b c d)] -> (ON4 a b c d -> x ot) -> WithMaskedObject x ot
  M5 :: (TypeableOT2 ot x, Inst5 IsObjectType a b c d e) => [Requirement (ON5 a b c d e)] -> (ON5 a b c d e -> x ot) -> WithMaskedObject x ot
  deriving (Typeable)

instance ConsIndex (WithMaskedObject x ot) where
  consIndex = \case
    M1 {} -> 1
    M2 {} -> 2
    M3 {} -> 3
    M4 {} -> 4
    M5 {} -> 5

data WithThis :: (Type -> Type) -> Type -> Type where
  T1 :: Inst1 IsObjectType a => (ON1 a -> x (ON1 a)) -> WithThis x (ON1 a)
  T2 :: Inst2 IsObjectType a b => (ON2 a b -> x (ON2 a b)) -> WithThis x (ON2 a b)
  T3 :: Inst3 IsObjectType a b c => (ON3 a b c -> x (ON3 a b c)) -> WithThis x (ON3 a b c)
  T4 :: Inst4 IsObjectType a b c d => (ON4 a b c d -> x (ON4 a b c d)) -> WithThis x (ON4 a b c d)
  T5 :: Inst5 IsObjectType a b c d e => (ON5 a b c d e -> x (ON5 a b c d e)) -> WithThis x (ON5 a b c d e)
  deriving (Typeable)

instance ConsIndex (WithThis x ot) where
  consIndex = \case
    T1 {} -> 1
    T2 {} -> 2
    T3 {} -> 3
    T4 {} -> 4
    T5 {} -> 5
