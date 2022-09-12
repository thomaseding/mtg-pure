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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module MtgPure.Model.Recursive (
  IsOT,
  IsZO,
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
  WithLinkedObject (..),
  WithMaskedObject (..),
  WithThis (..),
) where

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
import safe MtgPure.Model.ObjectType (OT1, OT2, OT3, OT4, OT5)
import safe MtgPure.Model.ObjectType.Any (WAny)
import safe MtgPure.Model.ObjectType.Card (WCard)
import safe MtgPure.Model.ObjectType.Index (IndexOT)
import safe MtgPure.Model.ObjectType.Kind (
  OTArtifact,
  OTArtifactCreature,
  OTCreature,
  OTEnchantment,
  OTEnchantmentCreature,
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
import safe MtgPure.Model.ZoneObject (
  OActivatedOrTriggeredAbility,
  OAny,
  OCreature,
  OCreaturePlayerPlaneswalker,
  ODamageSource,
  OPermanent,
  OPlayer,
  OSpell,
  ZO,
 )

-- Semantics legend:
--
-- "ot is (at least) all of (a,b,c,...)"
--    Ability ot
--    Card ot
--    WithThis zone litfOT ot
--
-- "ot is (at least) one of (a,b,c,...)"
--    Requirement zone ot
--    ZO zone ot

type IsOT (ot :: Type) = (IndexOT ot, VisitObjectN ot, PrettyType ot)

type IsZO (zone :: Zone) (ot :: Type) =
  (IsOT ot, IsZone zone, PrettyType (ZO zone ot))

data Ability (ot :: Type) :: Type where
  Activated :: Elect (Cost ot) ot -> Elect (Effect 'OneShot) ot -> Ability ot
  Static :: StaticAbility ot -> Ability ot
  Triggered :: TriggeredAbility ot -> Ability ot
  deriving (Typeable)

instance ConsIndex (Ability ot) where
  consIndex = \case
    Activated{} -> 1
    Static{} -> 2
    Triggered{} -> 3

data Card (ot :: Type) :: Type where
  -- For now Instants and Sorceries will use 'Battlefield for it's THIS zone.
  Card :: IsOT ot => CardName -> WCard ot -> WithThis 'Battlefield (CardTypeDef 'NonTribal) ot -> Card ot
  TribalCard :: IsOT ot => CardName -> WCard ot -> WithThis 'Battlefield (CardTypeDef 'Tribal) ot -> Card ot
  --
  ArtifactCard :: Card OTArtifact -> Card ()
  ArtifactCreatureCard :: Card OTArtifactCreature -> Card ()
  CreatureCard :: Card OTCreature -> Card ()
  EnchantmentCard :: Card OTEnchantment -> Card ()
  EnchantmentCreatureCard :: Card OTEnchantmentCreature -> Card ()
  InstantCard :: Card OTInstant -> Card ()
  LandCard :: Card OTLand -> Card ()
  PlaneswalkerCard :: Card OTPlaneswalker -> Card ()
  SorceryCard :: Card OTSorcery -> Card ()
  deriving (Typeable)

instance ConsIndex (Card ot) where
  consIndex = \case
    Card{} -> 1
    TribalCard{} -> 2
    ArtifactCard{} -> 3
    ArtifactCreatureCard{} -> 4
    CreatureCard{} -> 5
    EnchantmentCard{} -> 6
    EnchantmentCreatureCard{} -> 7
    InstantCard{} -> 8
    LandCard{} -> 9
    PlaneswalkerCard{} -> 10
    SorceryCard{} -> 11

data CardTypeDef (tribal :: Tribal) (ot :: Type) :: Type where
  ArtifactDef ::
    Colors ->
    Elect (Cost OTArtifact) OTArtifact ->
    [Ability OTArtifact] ->
    CardTypeDef 'NonTribal OTArtifact
  ArtifactCreatureDef ::
    Colors ->
    Elect (Cost OTArtifactCreature) OTArtifactCreature ->
    [CreatureType] ->
    Power ->
    Toughness ->
    [Ability OTArtifact] ->
    [Ability OTCreature] ->
    -- [Ability OTArtifactCreature] -> -- I don't think there exist any abilities in MTG yet that appear only on cards that are both artifacts and creatures.
    CardTypeDef 'NonTribal OTArtifactCreature
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
  EnchantmentCreatureDef ::
    Colors ->
    Elect (Cost OTEnchantmentCreature) OTEnchantmentCreature ->
    [CreatureType] ->
    Power ->
    Toughness ->
    [Ability OTCreature] ->
    [Ability OTEnchantment] ->
    [Ability OTEnchantmentCreature] ->
    CardTypeDef 'NonTribal OTEnchantmentCreature
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
    ArtifactCreatureDef{} -> 1
    ArtifactDef{} -> 2
    CreatureDef{} -> 3
    EnchantmentDef{} -> 4
    EnchantmentCreatureDef{} -> 5
    InstantDef{} -> 6
    LandDef{} -> 7
    PlaneswalkerDef{} -> 8
    SorceryDef{} -> 9
    TribalDef{} -> 10
    VariableDef{} -> 11

data Condition :: Type where
  CAnd :: [Condition] -> Condition
  CNot :: Condition -> Condition
  COr :: [Condition] -> Condition
  Satisfies :: IsZO zone ot => WAny ot -> ZO zone ot -> [Requirement zone ot] -> Condition
  deriving (Typeable)

instance ConsIndex Condition where
  consIndex = \case
    CAnd{} -> 1
    CNot{} -> 2
    COr{} -> 3
    Satisfies{} -> 4

data Cost (ot :: Type) :: Type where
  AndCosts :: [Cost ot] -> Cost ot
  DiscardRandomCost :: Int -> Cost ot -- TODO: PositiveInt
  LoyaltyCost :: Loyalty -> Cost OTPlaneswalker
  ManaCost :: ManaCost -> Cost ot
  OrCosts :: [Cost ot] -> Cost ot
  PayLife :: Int -> Cost ot -- TODO: PositiveInt
  SacrificeCost :: IsOT ot => WPermanent ot -> [Requirement 'Battlefield ot] -> Cost ot
  TapCost :: OPermanent -> Cost ot
  deriving (Typeable)

instance ConsIndex (Cost ot) where
  consIndex = \case
    AndCosts{} -> 1
    DiscardRandomCost{} -> 2
    LoyaltyCost{} -> 3
    ManaCost{} -> 4
    OrCosts{} -> 5
    PayLife{} -> 6
    SacrificeCost{} -> 7
    TapCost{} -> 8

data Effect (ef :: EffectType) :: Type where
  AddMana :: OPlayer -> ManaPool -> Effect 'OneShot
  AddToBattlefield :: IsOT ot => WPermanent ot -> OPlayer -> Token ot -> Effect 'OneShot
  ChangeTo :: IsOT ot => WPermanent ot -> OPermanent -> Card ot -> Effect 'Continuous
  CounterAbility :: OActivatedOrTriggeredAbility -> Effect 'OneShot
  CounterSpell :: OSpell -> Effect 'OneShot
  DealDamage :: ODamageSource -> OCreaturePlayerPlaneswalker -> Damage -> Effect 'OneShot
  Destroy :: OPermanent -> Effect 'OneShot
  DrawCards :: OPlayer -> Int -> Effect 'OneShot
  EffectContinuous :: Effect 'Continuous -> Effect 'OneShot -- 611.2
  EOr :: [Effect ef] -> Effect ef
  Gain :: IsOT ot => WAny ot -> ZO 'Battlefield ot -> Ability ot -> Effect 'Continuous
  Lose :: IsOT ot => WAny ot -> ZO 'Battlefield ot -> Ability ot -> Effect 'Continuous
  PutOntoBattlefield :: IsZO zone ot => WPermanent ot -> OPlayer -> ZO zone ot -> Effect 'OneShot -- TODO: zone /= 'Battlefield
  Sacrifice :: IsOT ot => WPermanent ot -> OPlayer -> [Requirement 'Battlefield ot] -> Effect 'OneShot
  SearchLibrary :: IsOT ot => WCard ot -> OPlayer -> WithLinkedObject 'Library (Elect (Effect 'OneShot)) ot -> Effect 'OneShot
  StatDelta :: OCreature -> Power -> Toughness -> Effect 'Continuous
  Until :: Elect Event OTPlayer -> Effect 'Continuous -> Effect 'Continuous
  deriving (Typeable)

instance ConsIndex (Effect ef) where
  consIndex = \case
    AddMana{} -> 1
    AddToBattlefield{} -> 2
    ChangeTo{} -> 3
    CounterAbility{} -> 4
    CounterSpell{} -> 5
    DealDamage{} -> 6
    Destroy{} -> 7
    EffectContinuous{} -> 8
    EOr{} -> 9
    DrawCards{} -> 10
    Gain{} -> 11
    Lose{} -> 12
    PutOntoBattlefield{} -> 13
    Sacrifice{} -> 14
    SearchLibrary{} -> 15
    StatDelta{} -> 16
    Until{} -> 17

data Elect (el :: Type) (ot :: Type) :: Type where
  A :: (el ~ Effect 'OneShot, IsZO zone ot) => Selection -> OPlayer -> WithMaskedObject zone (Elect el ot) -> Elect el ot
  ActivePlayer :: (OPlayer -> Elect el ot) -> Elect el ot
  -- TODO: Add `SZone zone` witness and change `'Battlefield` to `zone`.
  All :: IsOT ot => WithMaskedObject 'Battlefield (Elect el ot) -> Elect el ot
  Condition :: Condition -> Elect Condition ot
  ControllerOf :: OAny -> (OPlayer -> Elect el ot) -> Elect el ot
  Cost :: Cost ot -> Elect (Cost ot) ot
  Effect :: [Effect ef] -> Elect (Effect ef) ot
  Event :: Event -> Elect Event ot
  If :: Condition -> Elect el ot -> Else el ot -> Elect el ot
  Listen :: EventListener -> Elect EventListener ot
  -- TODO: Add `SZone zone` witness and change `'Battlefield` to `zone`.
  Random :: IsOT ot => WithMaskedObject 'Battlefield (Elect el ot) -> Elect el ot
  VariableFromPower :: OCreature -> (Variable -> Elect el ot) -> Elect el ot
  deriving (Typeable)

instance ConsIndex (Elect el ot) where
  consIndex = \case
    A{} -> 1
    ActivePlayer{} -> 2
    All{} -> 3
    Condition{} -> 4
    ControllerOf{} -> 5
    Cost{} -> 6
    Effect{} -> 7
    Event{} -> 8
    If{} -> 9
    Listen{} -> 10
    Random{} -> 11
    VariableFromPower{} -> 12

data Else (el :: Type) (ot :: Type) :: Type where
  ElseCost :: (el ~ Cost ot) => Elect el ot -> Else el ot
  ElseEffect :: (el ~ Effect 'OneShot) => Elect el ot -> Else el ot
  -- NB: Events need linear history to make sense of election costs tied to it, hence this hole.
  -- Imagine otherwise this were not the case. Then different parts of the branch could listen to different
  -- event types (without injecting yet another index/witness to prevent it). This is is dumb on its own
  -- and gets worse when the conditional has costs involved. You'd have to solve for the future to know what
  -- costs are paid in order to know which event to trigger! Impossible!
  ElseEvent :: (el ~ EventListener' liftOT) => Else el ot
  deriving (Typeable)

instance ConsIndex (Else el ot) where
  consIndex = \case
    ElseCost{} -> 1
    ElseEffect{} -> 2
    ElseEvent{} -> 3

type Event = EventListener' Proxy

type EventListener = EventListener' (Elect (Effect 'OneShot))

data EventListener' (liftOT :: Type -> Type) :: Type where
  BecomesTapped :: (IsOT ot, Typeable liftOT) => WPermanent ot -> WithLinkedObject 'Battlefield liftOT ot -> EventListener' liftOT
  Events :: [EventListener' liftOT] -> EventListener' liftOT
  SpellIsCast :: IsOT ot => WSpell ot -> WithLinkedObject 'Battlefield liftOT ot -> EventListener' liftOT
  TimePoint :: Typeable p => TimePoint p -> liftOT OTPlayer -> EventListener' liftOT
  deriving (Typeable)

instance ConsIndex (EventListener' liftOT) where
  consIndex = \case
    BecomesTapped{} -> 1
    Events{} -> 2
    SpellIsCast{} -> 3
    TimePoint{} -> 4

data NonProxy (liftOT :: Type -> Type) :: Type where
  NonProxyElectEffect :: NonProxy (Elect (Effect ef))

instance ConsIndex (NonProxy liftOT) where
  consIndex = \case
    NonProxyElectEffect -> 1

data Requirement (zone :: Zone) (ot :: Type) :: Type where
  ControlledBy :: OPlayer -> Requirement 'Battlefield ot
  HasAbility :: IsZO zone ot => WithThis zone Ability ot -> Requirement zone ot -- Non-unique differing representations will not be considered the same
  HasLandType :: LandType -> Requirement zone OTLand
  Is :: IsZO zone ot => WAny ot -> ZO zone ot -> Requirement zone ot
  Not :: IsZO zone ot => Requirement zone ot -> Requirement zone ot
  OfColors :: Colors -> Requirement zone ot -- needs `WCard a` witness
  OwnedBy :: OPlayer -> Requirement zone ot
  PlayerPays :: Cost OPlayer -> Requirement zone OTPlayer
  RAnd :: [Requirement zone ot] -> Requirement zone ot
  ROr :: [Requirement zone ot] -> Requirement zone ot
  Tapped :: IsOT ot => WPermanent ot -> Requirement 'Battlefield ot
  -- TODO: Try to add some combinators that go from: forall a b. [forall liftOT. Requirement x] -> Requirement (ON2 a, b)
  R2 ::
    Inst2 IsObjectType a b =>
    [Requirement zone (OT1 a)] ->
    [Requirement zone (OT1 b)] ->
    Requirement zone (OT2 a b)
  R3 ::
    Inst3 IsObjectType a b c =>
    [Requirement zone (OT1 a)] ->
    [Requirement zone (OT1 b)] ->
    [Requirement zone (OT1 c)] ->
    Requirement zone (OT3 a b c)
  R4 ::
    Inst4 IsObjectType a b c d =>
    [Requirement zone (OT1 a)] ->
    [Requirement zone (OT1 b)] ->
    [Requirement zone (OT1 c)] ->
    [Requirement zone (OT1 d)] ->
    Requirement zone (OT4 a b c d)
  R5 ::
    Inst5 IsObjectType a b c d e =>
    [Requirement zone (OT1 a)] ->
    [Requirement zone (OT1 b)] ->
    [Requirement zone (OT1 c)] ->
    [Requirement zone (OT1 d)] ->
    [Requirement zone (OT1 e)] ->
    Requirement zone (OT5 a b c d e)
  deriving (Typeable)

instance ConsIndex (Requirement zone ot) where
  consIndex = \case
    ControlledBy{} -> 1
    HasAbility{} -> 2
    HasLandType{} -> 3
    Is{} -> 4
    Not{} -> 5
    OfColors{} -> 6
    OwnedBy{} -> 7
    PlayerPays{} -> 8
    RAnd{} -> 9
    ROr{} -> 10
    Tapped{} -> 11
    R2{} -> 12
    R3{} -> 13
    R4{} -> 14
    R5{} -> 15

data SetCard (ot :: Type) :: Type where
  SetCard :: CardSet -> Rarity -> Card ot -> SetCard ot
  deriving (Typeable)

instance ConsIndex (SetCard ot) where
  consIndex = \case
    SetCard{} -> 1

data SetToken (ot :: Type) :: Type where
  SetToken :: CardSet -> Rarity -> Token ot -> SetToken ot
  deriving (Typeable)

instance ConsIndex (SetToken ot) where
  consIndex = \case
    SetToken{} -> 1

data StaticAbility (ot :: Type) :: Type where
  As :: IsOT ot => Elect EventListener ot -> StaticAbility ot -- 603.6d: not a triggered ability
  Bestow :: ot ~ OTEnchantmentCreature => Elect (Cost ot) ot -> StaticAbility ot
  Enchant :: IsZO zone ot => WithLinkedObject zone (Elect (Effect 'Continuous)) ot -> StaticAbility OTEnchantment
  FirstStrike :: StaticAbility OTCreature
  Flying :: StaticAbility OTCreature
  Haste :: StaticAbility OTCreature
  StaticContinuous :: Elect (Effect 'Continuous) ot -> StaticAbility ot -- 611.3
  Suspend :: Int -> Elect (Cost ot) ot -> StaticAbility ot -- PositiveInt
  deriving (Typeable)

instance ConsIndex (StaticAbility ot) where
  consIndex = \case
    As{} -> 1
    Bestow{} -> 2
    Enchant{} -> 3
    FirstStrike{} -> 4
    Flying{} -> 5
    Haste{} -> 6
    StaticContinuous{} -> 7
    Suspend{} -> 8

data Token (ot :: Type) :: Type where
  Token :: WPermanent ot -> Card ot -> Token ot
  ArtifactToken :: Token OTArtifact -> Token ()
  ArtifactCreatureToken :: Token OTArtifactCreature -> Token ()
  CreatureToken :: Token OTCreature -> Token ()
  EnchantmentToken :: Token OTEnchantment -> Token ()
  EnchantmentCreatureToken :: Token OTEnchantmentCreature -> Token ()
  LandToken :: Token OTLand -> Token ()
  PlaneswalkerToken :: Token OTPlaneswalker -> Token ()
  deriving (Typeable)

instance ConsIndex (Token ot) where
  consIndex = \case
    Token{} -> 1
    ArtifactToken{} -> 2
    ArtifactCreatureToken{} -> 3
    CreatureToken{} -> 4
    EnchantmentToken{} -> 5
    EnchantmentCreatureToken{} -> 6
    LandToken{} -> 7
    PlaneswalkerToken{} -> 8

-- https://www.mtgsalvation.com/forums/magic-fundamentals/magic-rulings/magic-rulings-archives/611601-whenever-what-does-it-mean?comment=3
-- https://www.reddit.com/r/magicTCG/comments/asmecb/noob_question_difference_between_as_and_when/
data TriggeredAbility (ot :: Type) :: Type where
  When :: IsOT ot => Elect EventListener ot -> TriggeredAbility ot
  deriving (Typeable)

instance ConsIndex (TriggeredAbility ot) where
  consIndex = \case
    When{} -> 1

data WithLinkedObject (zone :: Zone) (liftOT :: Type -> Type) (ot :: Type) :: Type where
  LProxy :: [Requirement zone ot] -> WithLinkedObject zone Proxy ot
  L1 ::
    (IsOT (OT1 a), Inst1 IsObjectType a) =>
    NonProxy liftOT ->
    [Requirement zone (OT1 a)] ->
    (ZO zone (OT1 a) -> liftOT (OT1 a)) ->
    WithLinkedObject zone liftOT (OT1 a)
  L2 ::
    (IsOT (OT2 a b), Inst2 IsObjectType a b) =>
    NonProxy liftOT ->
    [Requirement zone (OT2 a b)] ->
    (ZO zone (OT2 a b) -> liftOT (OT2 a b)) ->
    WithLinkedObject zone liftOT (OT2 a b)
  L3 ::
    (IsOT (OT3 a b c), Inst3 IsObjectType a b c) =>
    NonProxy liftOT ->
    [Requirement zone (OT3 a b c)] ->
    (ZO zone (OT3 a b c) -> liftOT (OT3 a b c)) ->
    WithLinkedObject zone liftOT (OT3 a b c)
  L4 ::
    (IsOT (OT4 a b c d), Inst4 IsObjectType a b c d) =>
    NonProxy liftOT ->
    [Requirement zone (OT4 a b c d)] ->
    (ZO zone (OT4 a b c d) -> liftOT (OT4 a b c d)) ->
    WithLinkedObject zone liftOT (OT4 a b c d)
  L5 ::
    (IsOT (OT5 a b c d e), Inst5 IsObjectType a b c d e) =>
    NonProxy liftOT ->
    [Requirement zone (OT5 a b c d e)] ->
    (ZO zone (OT5 a b c d e) -> liftOT (OT5 a b c d e)) ->
    WithLinkedObject zone liftOT (OT5 a b c d e)
  deriving (Typeable)

instance ConsIndex (WithLinkedObject zone liftOT ot) where
  consIndex = \case
    LProxy{} -> 1
    L1{} -> 2
    L2{} -> 3
    L3{} -> 4
    L4{} -> 5
    L5{} -> 6

-- XXX Can this be encoded as `WithLinkedObject` under the hood?
data WithMaskedObject (zone :: Zone) (liftedOT :: Type) :: Type where
  M1 ::
    (Typeable liftedOT, IsOT (OT1 a), Inst1 IsObjectType a) =>
    [Requirement zone (OT1 a)] ->
    (ZO zone (OT1 a) -> liftedOT) ->
    WithMaskedObject zone liftedOT
  M2 ::
    (Typeable liftedOT, IsOT (OT2 a b), Inst2 IsObjectType a b) =>
    [Requirement zone (OT2 a b)] ->
    (ZO zone (OT2 a b) -> liftedOT) ->
    WithMaskedObject zone liftedOT
  M3 ::
    (Typeable liftedOT, IsOT (OT3 a b c), Inst3 IsObjectType a b c) =>
    [Requirement zone (OT3 a b c)] ->
    (ZO zone (OT3 a b c) -> liftedOT) ->
    WithMaskedObject zone liftedOT
  M4 ::
    (Typeable liftedOT, IsOT (OT4 a b c d), Inst4 IsObjectType a b c d) =>
    [Requirement zone (OT4 a b c d)] ->
    (ZO zone (OT4 a b c d) -> liftedOT) ->
    WithMaskedObject zone liftedOT
  M5 ::
    (Typeable liftedOT, IsOT (OT5 a b c d e), Inst5 IsObjectType a b c d e) =>
    [Requirement zone (OT5 a b c d e)] ->
    (ZO zone (OT5 a b c d e) -> liftedOT) ->
    WithMaskedObject zone liftedOT
  deriving (Typeable)

instance ConsIndex (WithMaskedObject zone liftedOT) where
  consIndex = \case
    M1{} -> 1
    M2{} -> 2
    M3{} -> 3
    M4{} -> 4
    M5{} -> 5

data WithThis (zone :: Zone) (liftOT :: Type -> Type) (ot :: Type) :: Type where
  T1 ::
    (IsOT (OT1 a), Inst1 IsObjectType a) =>
    (ZO zone (OT1 a) -> liftOT (OT1 a)) ->
    WithThis zone liftOT (OT1 a)
  T2 ::
    (IsOT (OT2 a b), Inst2 IsObjectType a b) =>
    -- TODO: Add a additional full (ZO zone (OT2 a b)) to the input tuple?
    ((ZO zone (OT1 a), ZO zone (OT1 b)) -> liftOT (OT2 a b)) ->
    WithThis zone liftOT (OT2 a b)
  deriving (Typeable)

instance ConsIndex (WithThis zone liftOT ot) where
  consIndex = \case
    T1{} -> 1
    T2{} -> 2
