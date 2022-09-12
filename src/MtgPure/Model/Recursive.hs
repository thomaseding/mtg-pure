{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

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
    Else (..),
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
    WithLinkedCard (..),
    WithLinkedObject (..),
    WithMaskedCard (..),
    WithMaskedObject (..),
    WithThis (..),
    ZoneCard (..),
  )
where

import safe Data.ConsIndex (ConsIndex (..))
import safe Data.Inst (Inst1, Inst2, Inst3, Inst4, Inst5)
import safe Data.Kind (Type)
import safe Data.Proxy (Proxy (..))
import safe Data.Typeable (Typeable)
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
import safe MtgPure.Model.ObjectN (ObjectN)
import safe MtgPure.Model.ObjectN.Type
  ( OActivatedOrTriggeredAbility,
    OAny,
    OCreature,
    OCreaturePlayerPlaneswalker,
    ODamageSource,
    ON1,
    ON2,
    ON3,
    ON4,
    ON5,
    OPermanent,
    OPlayer,
    OSpell,
  )
import safe MtgPure.Model.ObjectType (OT1, OT2, OT3, OT4, OT5)
import safe MtgPure.Model.ObjectType.Any (WAny)
import safe MtgPure.Model.ObjectType.Card (WCard)
import safe MtgPure.Model.ObjectType.Index (IndexOT)
import safe MtgPure.Model.ObjectType.Kind
  ( OTArtifact,
    OTArtifactCreature,
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
import safe MtgPure.Model.ObjectType.Spell (WSpell (..))
import safe MtgPure.Model.Power (Power)
import safe MtgPure.Model.PrettyType (PrettyType (..))
import safe MtgPure.Model.Rarity (Rarity)
import safe MtgPure.Model.Selection (Selection)
import safe MtgPure.Model.TimePoint (TimePoint)
import safe MtgPure.Model.Toughness (Toughness)
import safe MtgPure.Model.Tribal (Tribal (..))
import safe MtgPure.Model.Variable (Variable)
import safe MtgPure.Model.VisitObjectN (VisitObjectN)
import safe MtgPure.Model.Zone (IsZone (..), Zone (..))

type TypeableOT ot =
  ( Typeable ot,
    IndexOT ot,
    VisitObjectN ot,
    PrettyType ot,
    PrettyType (ObjectN ot)
  )

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
  Card :: CardName -> WCard ot -> WithThis (CardTypeDef 'NonTribal) ot -> Card ot
  TribalCard :: CardName -> WCard ot -> WithThis (CardTypeDef 'Tribal) ot -> Card ot
  --
  ArtifactCard :: Card OTArtifact -> Card ()
  CreatureCard :: Card OTCreature -> Card ()
  EnchantmentCard :: Card OTEnchantment -> Card ()
  InstantCard :: Card OTInstant -> Card ()
  LandCard :: Card OTLand -> Card ()
  PlaneswalkerCard :: Card OTPlaneswalker -> Card ()
  SorceryCard :: Card OTSorcery -> Card ()
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
    [LandType] ->
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
  CNot :: Condition -> Condition
  COr :: [Condition] -> Condition
  Satisfies :: TypeableOT ot => WAny ot -> ObjectN ot -> [Requirement (ObjectN ot)] -> Condition
  deriving (Typeable)

instance ConsIndex Condition where
  consIndex = \case
    CAnd {} -> 1
    CNot {} -> 2
    COr {} -> 3
    Satisfies {} -> 4

data Cost :: Type -> Type where
  AndCosts :: [Cost ot] -> Cost ot
  DiscardRandomCost :: Int -> Cost ot -- TODO: PositiveInt
  LoyaltyCost :: Loyalty -> Cost OTPlaneswalker
  ManaCost :: ManaCost -> Cost ot
  OrCosts :: [Cost ot] -> Cost ot
  PayLife :: Int -> Cost ot -- TODO: PositiveInt
  SacrificeCost :: TypeableOT ot => WPermanent ot -> [Requirement (ObjectN ot)] -> Cost ot
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
  Gain :: TypeableOT ot => WAny ot -> ObjectN ot -> Ability ot -> Effect 'Continuous
  Lose :: TypeableOT ot => WAny ot -> ObjectN ot -> Ability ot -> Effect 'Continuous
  PutOntoBattlefield :: TypeableOT ot => WPermanent ot -> OPlayer -> ZoneCard 'LibraryZone ot -> Effect 'OneShot
  Sacrifice :: TypeableOT ot => WPermanent ot -> OPlayer -> [Requirement (ObjectN ot)] -> Effect 'OneShot
  SearchLibrary :: TypeableOT ot => WCard ot -> OPlayer -> WithLinkedCard 'LibraryZone (Elect (Effect 'OneShot)) ot -> Effect 'OneShot
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
    PutOntoBattlefield {} -> 13
    Sacrifice {} -> 14
    SearchLibrary {} -> 15
    StatDelta {} -> 16
    Until {} -> 17

data Elect :: Type -> Type -> Type where
  A :: (e ~ Effect 'OneShot) => Selection -> OPlayer -> WithMaskedObject (Elect e ot) -> Elect e ot
  ActivePlayer :: (OPlayer -> Elect e ot) -> Elect e ot
  All :: WithMaskedObject (Elect e ot) -> Elect e ot
  Condition :: Condition -> Elect Condition ot
  ControllerOf :: OAny -> (OPlayer -> Elect e ot) -> Elect e ot
  Cost :: Cost ot -> Elect (Cost ot) ot
  Effect :: [Effect e] -> Elect (Effect e) ot
  Event :: Event -> Elect Event ot
  If :: Condition -> Elect e ot -> Else e ot -> Elect e ot
  Listen :: EventListener -> Elect EventListener ot
  Random :: TypeableOT2 ot (Elect e) => WithMaskedObject (Elect e ot) -> Elect e ot
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

data Else :: Type -> Type -> Type where
  ElseCost :: (e ~ Cost ot) => Elect e ot -> Else e ot
  ElseEffect :: (e ~ Effect 'OneShot) => Elect e ot -> Else e ot
  ElseEvent :: Else (EventListener' x) ot -- NB: Events need linear history to make sense of election costs tied to it, hence this hole
  deriving (Typeable)

instance ConsIndex (Else e ot) where
  consIndex = \case
    ElseCost {} -> 1
    ElseEffect {} -> 2
    ElseEvent {} -> 3

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

-- Idea is to allow both these:
data Requirement :: Type -> Type where
  ControlledBy :: OPlayer -> Requirement (ObjectN ot)
  HasAbility :: WithThis Ability ot -> Requirement (ObjectN ot) -- Non-unique differing representations will not be considered the same
  HasLandType :: LandType -> Requirement (x OTLand)
  Impossible :: Requirement (x ot)
  Is :: TypeableOT ot => WAny ot -> ObjectN ot -> Requirement (ObjectN ot)
  Not :: TypeableOT ot => Requirement (x ot) -> Requirement (x ot)
  OfColors :: Colors -> Requirement (x ot) -- needs `WCard a` witness
  OwnedBy :: OPlayer -> Requirement (ObjectN ot)
  PlayerPays :: Cost OPlayer -> Requirement (ObjectN OTPlayer)
  RAnd :: [Requirement (x ot)] -> Requirement (x ot)
  ROr :: [Requirement (x ot)] -> Requirement (x ot)
  Tapped :: TypeableOT ot => WPermanent ot -> Requirement (ObjectN ot)
  -- TODO: Try to add some combinators that go from: forall a b. [forall x. Requirement x] -> Requirement (ON2 a, b)
  R2 :: Inst2 IsObjectType a b => [Requirement (x (OT1 a))] -> [Requirement (x (OT1 b))] -> Requirement (x (OT2 a b))
  R3 :: Inst3 IsObjectType a b c => [Requirement (x (OT1 a))] -> [Requirement (x (OT1 b))] -> [Requirement (x (OT1 c))] -> Requirement (x (OT3 a b c))
  R4 :: Inst4 IsObjectType a b c d => [Requirement (x (OT1 a))] -> [Requirement (x (OT1 b))] -> [Requirement (x (OT1 c))] -> [Requirement (x (OT1 d))] -> Requirement (x (OT4 a b c d))
  R5 :: Inst5 IsObjectType a b c d e => [Requirement (x (OT1 a))] -> [Requirement (x (OT1 b))] -> [Requirement (x (OT1 c))] -> [Requirement (x (OT1 d))] -> [Requirement (x (OT1 e))] -> Requirement (x (OT5 a b c d e))
  deriving (Typeable)

instance ConsIndex (Requirement a) where
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
  FirstStrike :: StaticAbility OTCreature
  Flying :: StaticAbility OTCreature
  Haste :: StaticAbility OTCreature
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

data Token :: Type -> Type where
  Token :: WPermanent ot -> Card ot -> Token ot
  ArtifactToken :: Token OTArtifact -> Token ()
  CreatureToken :: Token OTCreature -> Token ()
  EnchantmentToken :: Token OTEnchantment -> Token ()
  LandToken :: Token OTLand -> Token ()
  PlaneswalkerToken :: Token OTPlaneswalker -> Token ()
  deriving (Typeable)

instance ConsIndex (Token ot) where
  consIndex = \case
    Token {} -> 1
    ArtifactToken {} -> 2
    CreatureToken {} -> 3
    EnchantmentToken {} -> 4
    LandToken {} -> 5
    PlaneswalkerToken {} -> 6

-- https://www.mtgsalvation.com/forums/magic-fundamentals/magic-rulings/magic-rulings-archives/611601-whenever-what-does-it-mean?comment=3
-- https://www.reddit.com/r/magicTCG/comments/asmecb/noob_question_difference_between_as_and_when/
data TriggeredAbility :: Type -> Type where
  When :: TypeableOT ot => Elect EventListener ot -> TriggeredAbility ot
  deriving (Typeable)

instance ConsIndex (TriggeredAbility ot) where
  consIndex = \case
    When {} -> 1

data WithLinkedCard :: Zone -> (Type -> Type) -> Type -> Type where
  LcProxy :: [Requirement (ZoneCard zone ot)] -> WithLinkedCard zone Proxy ot
  Lc1 :: (TypeableOT (OT1 a), Inst1 IsObjectType a) => NonProxy x -> [Requirement (ZoneCard zone (OT1 a))] -> (ZoneCard zone (OT1 a) -> x (OT1 a)) -> WithLinkedCard zone x (OT1 a)
  Lc2 :: (TypeableOT (OT2 a b), Inst2 IsObjectType a b) => NonProxy x -> [Requirement (ZoneCard zone (OT2 a b))] -> (ZoneCard zone (OT2 a b) -> x (OT2 a b)) -> WithLinkedCard zone x (OT2 a b)
  Lc3 :: (TypeableOT (OT3 a b c), Inst3 IsObjectType a b c) => NonProxy x -> [Requirement (ZoneCard zone (OT3 a b c))] -> (ZoneCard zone (OT3 a b c) -> x (OT3 a b c)) -> WithLinkedCard zone x (OT3 a b c)
  Lc4 :: (TypeableOT (OT4 a b c d), Inst4 IsObjectType a b c d) => NonProxy x -> [Requirement (ZoneCard zone (OT4 a b c d))] -> (ZoneCard zone (OT4 a b c d) -> x (OT4 a b c d)) -> WithLinkedCard zone x (OT4 a b c d)
  Lc5 :: (TypeableOT (OT5 a b c d e), Inst5 IsObjectType a b c d e) => NonProxy x -> [Requirement (ZoneCard zone (OT5 a b c d e))] -> (ZoneCard zone (OT5 a b c d e) -> x (OT5 a b c d e)) -> WithLinkedCard zone x (OT5 a b c d e)
  deriving (Typeable)

instance ConsIndex (WithLinkedCard zone x ot) where
  consIndex = \case
    LcProxy {} -> 1
    Lc1 {} -> 2
    Lc2 {} -> 3
    Lc3 {} -> 4
    Lc4 {} -> 5
    Lc5 {} -> 6

data WithLinkedObject :: (Type -> Type) -> Type -> Type where
  LProxy :: [Requirement (ObjectN ot)] -> WithLinkedObject Proxy ot
  L1 :: (TypeableOT (OT1 a), Inst1 IsObjectType a) => NonProxy x -> [Requirement (ON1 a)] -> (ON1 a -> x (OT1 a)) -> WithLinkedObject x (OT1 a)
  L2 :: (TypeableOT (OT2 a b), Inst2 IsObjectType a b) => NonProxy x -> [Requirement (ON2 a b)] -> (ON2 a b -> x (OT2 a b)) -> WithLinkedObject x (OT2 a b)
  L3 :: (TypeableOT (OT3 a b c), Inst3 IsObjectType a b c) => NonProxy x -> [Requirement (ON3 a b c)] -> (ON3 a b c -> x (OT3 a b c)) -> WithLinkedObject x (OT3 a b c)
  L4 :: (TypeableOT (OT4 a b c d), Inst4 IsObjectType a b c d) => NonProxy x -> [Requirement (ON4 a b c d)] -> (ON4 a b c d -> x (OT4 a b c d)) -> WithLinkedObject x (OT4 a b c d)
  L5 :: (TypeableOT (OT5 a b c d e), Inst5 IsObjectType a b c d e) => NonProxy x -> [Requirement (ON5 a b c d e)] -> (ON5 a b c d e -> x (OT5 a b c d e)) -> WithLinkedObject x (OT5 a b c d e)
  deriving (Typeable)

instance ConsIndex (WithLinkedObject x ot) where
  consIndex = \case
    LProxy {} -> 1
    L1 {} -> 2
    L2 {} -> 3
    L3 {} -> 4
    L4 {} -> 5
    L5 {} -> 6

data WithMaskedCard :: Zone -> Type -> Type where
  Mc1 :: (Typeable x, TypeableOT (OT1 a), Inst1 IsObjectType a) => [Requirement (ZoneCard zone (OT1 a))] -> (ZoneCard zone (OT1 a) -> x) -> WithMaskedCard zone x
  Mc2 :: (Typeable x, TypeableOT (OT2 a b), Inst2 IsObjectType a b) => [Requirement (ZoneCard zone (OT2 a b))] -> (ZoneCard zone (OT2 a b) -> x) -> WithMaskedCard zone x
  Mc3 :: (Typeable x, TypeableOT (OT3 a b c), Inst3 IsObjectType a b c) => [Requirement (ZoneCard zone (OT3 a b c))] -> (ZoneCard zone (OT3 a b c) -> x) -> WithMaskedCard zone x
  Mc4 :: (Typeable x, TypeableOT (OT4 a b c d), Inst4 IsObjectType a b c d) => [Requirement (ZoneCard zone (OT4 a b c d))] -> (ZoneCard zone (OT4 a b c d) -> x) -> WithMaskedCard zone x
  Mc5 :: (Typeable x, TypeableOT (OT5 a b c d e), Inst5 IsObjectType a b c d e) => [Requirement (ZoneCard zone (OT5 a b c d e))] -> (ZoneCard zone (OT5 a b c d e) -> x) -> WithMaskedCard zone x
  deriving (Typeable)

instance ConsIndex (WithMaskedCard zone x) where
  consIndex = \case
    Mc1 {} -> 1
    Mc2 {} -> 2
    Mc3 {} -> 3
    Mc4 {} -> 4
    Mc5 {} -> 5

data WithMaskedObject :: Type -> Type where
  M1 :: (Typeable x, TypeableOT (OT1 a), Inst1 IsObjectType a) => [Requirement (ON1 a)] -> (ON1 a -> x) -> WithMaskedObject x
  M2 :: (Typeable x, TypeableOT (OT2 a b), Inst2 IsObjectType a b) => [Requirement (ON2 a b)] -> (ON2 a b -> x) -> WithMaskedObject x
  M3 :: (Typeable x, TypeableOT (OT3 a b c), Inst3 IsObjectType a b c) => [Requirement (ON3 a b c)] -> (ON3 a b c -> x) -> WithMaskedObject x
  M4 :: (Typeable x, TypeableOT (OT4 a b c d), Inst4 IsObjectType a b c d) => [Requirement (ON4 a b c d)] -> (ON4 a b c d -> x) -> WithMaskedObject x
  M5 :: (Typeable x, TypeableOT (OT5 a b c d e), Inst5 IsObjectType a b c d e) => [Requirement (ON5 a b c d e)] -> (ON5 a b c d e -> x) -> WithMaskedObject x
  deriving (Typeable)

instance ConsIndex (WithMaskedObject x) where
  consIndex = \case
    M1 {} -> 1
    M2 {} -> 2
    M3 {} -> 3
    M4 {} -> 4
    M5 {} -> 5

data WithThis :: (Type -> Type) -> Type -> Type where
  T1 :: (TypeableOT (OT1 a), Inst1 IsObjectType a) => (ON1 a -> x (OT1 a)) -> WithThis x (OT1 a)
  T2 :: (TypeableOT (OT2 a b), Inst2 IsObjectType a b) => (ON2 a b -> x (OT2 a b)) -> WithThis x (OT2 a b)
  T3 :: (TypeableOT (OT3 a b c), Inst3 IsObjectType a b c) => (ON3 a b c -> x (OT3 a b c)) -> WithThis x (OT3 a b c)
  T4 :: (TypeableOT (OT4 a b c d), Inst4 IsObjectType a b c d) => (ON4 a b c d -> x (OT4 a b c d)) -> WithThis x (OT4 a b c d)
  T5 :: (TypeableOT (OT5 a b c d e), Inst5 IsObjectType a b c d e) => (ON5 a b c d e -> x (OT5 a b c d e)) -> WithThis x (OT5 a b c d e)
  deriving (Typeable)

instance ConsIndex (WithThis x ot) where
  consIndex = \case
    T1 {} -> 1
    T2 {} -> 2
    T3 {} -> 3
    T4 {} -> 4
    T5 {} -> 5

data ZoneCard :: Zone -> Type -> Type where
  LibraryCard :: TypeableOT ot => ObjectN ot -> ZoneCard 'LibraryZone ot
  deriving (Typeable)

instance ConsIndex (ZoneCard zone ot) where
  consIndex = \case
    LibraryCard {} -> 1

instance (IsZone zone, PrettyType ot) => PrettyType (ZoneCard zone ot) where
  prettyType _ = "ZoneCard '" ++ sZone ++ " " ++ open ++ sOT ++ close
    where
      sZone = show $ litZone (Proxy @zone)
      sOT = prettyType (Proxy @ot)
      (open, close) = case ' ' `elem` sOT of
        True -> ("(", ")")
        False -> ("", "")
