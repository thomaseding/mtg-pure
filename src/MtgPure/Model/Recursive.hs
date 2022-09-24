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
  Ability (..),
  ActivatedAbility (..),
  Card (..),
  CardTypeDef (..),
  Case (..),
  Condition (..),
  Cost (..),
  Effect (..),
  Elect (..),
  ElectPrePost,
  Else (..),
  ElsePrePost,
  Enchant (..),
  EnchantmentType (..),
  Event,
  EventListener,
  EventListener' (..),
  NonProxy (..),
  Requirement (..),
  SetCard (..),
  SetToken (..),
  Some (..),
  SomeCard,
  SomeCardOrToken,
  SomeTerm (..),
  SomeToken,
  StaticAbility (..),
  Token (..),
  TriggeredAbility (..),
  WithLinkedObject (..),
  WithMaskedObject (..),
  WithThis (..),
) where

import safe Data.ConsIndex (ConsIndex (..))
import safe Data.Inst (Inst1, Inst2, Inst3, Inst4, Inst5, Inst6)
import safe Data.Kind (Type)
import safe Data.Proxy (Proxy (..))
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.CardName (CardName)
import safe MtgPure.Model.CardSet (CardSet)
import safe MtgPure.Model.Color (Color)
import safe MtgPure.Model.Colors (Colors)
import safe MtgPure.Model.CreatureType (CreatureType)
import safe MtgPure.Model.Damage (Damage)
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.LandType (LandType)
import safe MtgPure.Model.Loyalty (Loyalty)
import safe MtgPure.Model.Mana (Snow (..))
import safe MtgPure.Model.ManaCost (ManaCost)
import safe MtgPure.Model.ManaPool (ManaPool)
import safe MtgPure.Model.Object (IsObjectType, OT1, OT2, OT3, OT4, OT5, OT6)
import safe MtgPure.Model.ObjectType.Any (WAny)
import safe MtgPure.Model.ObjectType.Card (WCard)
import safe MtgPure.Model.ObjectType.Kind (
  OTActivatedOrTriggeredAbility,
  OTAny,
  OTArtifact,
  OTArtifactCreature,
  OTArtifactLand,
  OTCreature,
  OTEnchantment,
  OTEnchantmentCreature,
  OTInstant,
  OTLand,
  OTPlaneswalker,
  OTPlayer,
  OTSorcery,
  OTSpell,
 )
import safe MtgPure.Model.ObjectType.NonCreatureCard (WNonCreatureCard)
import safe MtgPure.Model.ObjectType.Permanent (WPermanent)
import safe MtgPure.Model.ObjectType.Spell (WSpell (..))
import safe MtgPure.Model.Power (Power)
import safe MtgPure.Model.PrePost (IsPrePost, PrePost (..))
import safe MtgPure.Model.Rarity (Rarity)
import safe MtgPure.Model.TimePoint (TimePoint)
import safe MtgPure.Model.Toughness (Toughness)
import safe MtgPure.Model.Tribal (IsTribal, Tribal (..))
import safe MtgPure.Model.Variable (Var (Var), Variable)
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject (
  IsOT,
  IsZO,
  OCreature,
  OCreaturePlayerPlaneswalker,
  ODamageSource,
  OPermanent,
  OPlayer,
  ZO,
 )

----------------------------------------

-- Semantics legend:
--
-- "ot is exactly (a,b,c,...)"
--    Ability ot
--    Card ot
--    EnchantmentType ot
--    WithThis zone litfOT ot
--
-- "ot is (at least) one of (a,b,c,...)"
--    Enchant zone ot
--    Requirement zone ot
--    Some liftOT ot
--    ZO zone ot

----------------------------------------

data Ability (ot :: Type) :: Type where
  Activated :: IsZO zone ot => ActivatedAbility zone ot -> Ability ot
  Static :: IsZO zone ot => StaticAbility zone ot -> Ability ot
  Triggered :: IsZO zone ot => TriggeredAbility zone ot -> Ability ot
  deriving (Typeable)

instance ConsIndex (Ability ot) where
  consIndex = \case
    Activated{} -> 1
    Static{} -> 2
    Triggered{} -> 3

----------------------------------------

data ActivatedAbility (zone :: Zone) (ot :: Type) :: Type where
  Ability :: IsOT ot => ElectPrePost (Cost ot) ot -> ElectPrePost (Effect 'OneShot) ot -> ActivatedAbility 'ZBattlefield ot
  --GraveyardAbility :: WithThis 'ZGraveyard ot etc
  deriving (Typeable)

instance ConsIndex (ActivatedAbility zone ot) where
  consIndex = \case
    Ability{} -> 1

----------------------------------------

-- data ArtifactType (ot :: Type) :: Type where
--   Vehicle :: ArtifactType ot -- TODO: Stuff crew mechanic into here
--   deriving (Bounded, Enum, Eq, Ord, Show)

----------------------------------------

data Card (ot :: Type) :: Type where
  -- For now Instants and Sorceries will use `ZBattlefield` for it's `this` zone while the spell is resolving.
  -- If it's on the stack, it's `ZStack` as expected.
  -- If I need references to `this` from other zones, add an appropriate constructor that has a `WithThis theZone`,
  -- such something like `SomethingThatNeedsGraveyardThis :: WithThis 'ZGraveyard (Elect (Cost ot)) ot :: Ability ot`
  Card :: IsOT ot => CardName -> WCard ot -> WithThis 'ZBattlefield (Elect 'Pre (CardTypeDef 'NonTribal ot)) ot -> Card ot
  TribalCard :: IsOT ot => CardName -> WCard ot -> WithThis 'ZBattlefield (Elect 'Pre (CardTypeDef 'Tribal ot)) ot -> Card ot
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

----------------------------------------

data CardTypeDef (tribal :: Tribal) (ot :: Type) :: Type where
  ArtifactDef ::
    { artifact_colors :: Colors
    , artifact_cost :: ElectPrePost (Cost OTArtifact) OTArtifact
    , artifact_abilities :: [Ability OTArtifact]
    } ->
    CardTypeDef 'NonTribal OTArtifact
  ArtifactCreatureDef ::
    { artifactCreature_colors :: Colors
    , artifactCreature_cost :: ElectPrePost (Cost OTArtifactCreature) OTArtifactCreature
    , artifactCreature_creatureTypes :: [CreatureType]
    , artifactCreature_power :: Power
    , artifactCreature_toughness :: Toughness
    , artifactCreature_artifactAbilities :: [Ability OTArtifact]
    , artifactCreature_creatureAbilities :: [Ability OTCreature]
    -- , artifactCreature_artifactCreatureAbilities :: [Ability OTArtifactCreature]
    -- , artifactCreature_artifactTypes :: [ArtifactType]
    } ->
    CardTypeDef 'NonTribal OTArtifactCreature
  ArtifactLandDef ::
    { artifactLand_landTypes :: [LandType]
    , artifactLand_artifactAbilities :: [Ability OTArtifact]
    , artifactLand_landAbilities :: [Ability OTLand]
    -- , artifactLand_artifactLandAbilities :: [Ability OTArtifactLand]
    -- , artifactLand_artifactTypes :: [ArtifactType]
    } ->
    CardTypeDef 'NonTribal OTArtifactLand
  CreatureDef ::
    { creature_colors :: Colors
    , creature_cost :: ElectPrePost (Cost OTCreature) OTCreature
    , creature_subtypes :: [CreatureType]
    , creature_power :: Power
    , creature_toughness :: Toughness
    , creature_abilities :: [Ability OTCreature]
    } ->
    CardTypeDef 'NonTribal OTCreature
  EnchantmentDef ::
    { enchantment_colors :: Colors
    , enchantment_cost :: ElectPrePost (Cost OTEnchantment) OTEnchantment
    , enchantment_subtypes :: [EnchantmentType OTEnchantment]
    , enchantment_abilities :: [Ability OTEnchantment]
    } ->
    CardTypeDef 'NonTribal OTEnchantment
  EnchantmentCreatureDef ::
    { enchantmentCreature_colors :: Colors
    , enchantmentCreature_cost :: ElectPrePost (Cost OTEnchantmentCreature) OTEnchantmentCreature
    , enchantmentCreature_creatureTypes :: [CreatureType]
    , enchantmentCreature_power :: Power
    , enchantmentCreature_toughness :: Toughness
    , enchantmentCreature_creatureAbilities :: [Ability OTCreature]
    , enchantmentCreature_enchantmentAbilities :: [Ability OTEnchantment]
    , enchantmentCreature_enchantmentCreatureAbilities :: [Ability OTEnchantmentCreature]
    -- , enchantmentCreature_enchantmentTypes :: [EnchantmentType OTEnchantmentCreature]
    } ->
    CardTypeDef 'NonTribal OTEnchantmentCreature
  InstantDef ::
    { instant_colors :: Colors
    , instant_cost :: ElectPrePost (Cost OTInstant) OTInstant
    , instant_abilities :: [Ability OTInstant]
    , instant_effect :: ElectPrePost (Effect 'OneShot) OTInstant
    } ->
    CardTypeDef 'NonTribal OTInstant
  LandDef ::
    { land_subtypes :: [LandType]
    , land_abilities :: [Ability OTLand]
    } ->
    CardTypeDef 'NonTribal OTLand
  PlaneswalkerDef ::
    { planeswalker_colors :: Colors
    , planeswalker_cost :: ElectPrePost (Cost OTPlaneswalker) OTPlaneswalker
    , planeswalker_loyalty :: Loyalty
    , planeswalker_abilities :: [Ability OTPlaneswalker]
    } ->
    CardTypeDef 'NonTribal OTPlaneswalker
  SorceryDef ::
    { sorcery_colors :: Colors
    , sorcery_cost :: ElectPrePost (Cost OTSorcery) OTSorcery
    , sorcery_abilities :: [Ability OTSorcery]
    , sorcery_effect :: ElectPrePost (Effect 'OneShot) OTSorcery
    } ->
    CardTypeDef 'NonTribal OTSorcery
  TribalDef ::
    { tribal_subtypes :: [CreatureType]
    , tribal_witness :: WNonCreatureCard ot
    , tribal_def :: CardTypeDef 'NonTribal ot
    } ->
    CardTypeDef 'Tribal ot
  VariableDef ::
    (Variable Int -> CardTypeDef tribal ot) ->
    CardTypeDef tribal ot
  deriving (Typeable)

instance ConsIndex (CardTypeDef tribe ot) where
  consIndex = \case
    ArtifactDef{} -> 1
    ArtifactCreatureDef{} -> 2
    ArtifactLandDef{} -> 3
    CreatureDef{} -> 4
    EnchantmentCreatureDef{} -> 5
    EnchantmentDef{} -> 6
    InstantDef{} -> 7
    LandDef{} -> 8
    PlaneswalkerDef{} -> 9
    SorceryDef{} -> 10
    TribalDef{} -> 11
    VariableDef{} -> 12

----------------------------------------

data Case x where
  CaseColor ::
    { caseColor :: Variable Color
    , ofWhite :: x
    , ofBlue :: x
    , ofBlack :: x
    , ofRed :: x
    , ofGreen :: x
    } ->
    Case x
  deriving (Typeable)

instance ConsIndex (Case x) where
  consIndex = \case
    CaseColor{} -> 1

----------------------------------------

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

----------------------------------------

-- XXX: Uggh... need to add another type index for what to do after since some effects and abilities need to
-- know which costs were paid. (e.g. "If black mana was spend to pay this card's cost then ..."). Then a
-- continuation constructor needs to be provided. Then constructors of other types that use costs and something
-- else need to be updated accordingly. Ponder a less intrusive and less annoying solution... Perhaps Effect
-- (or whatever types) gets a constructor that can obtain an abtract runtime Cost which can be queried by
-- the API. This would likely require the model to encode more contingencies to handle dynamic issues, but this
-- is likely overwhelmingly worth it to avoid the continuation approach.
data Cost (ot :: Type) :: Type where
  AndCosts :: [Cost ot] -> Cost ot
  DiscardRandomCost :: Int -> Cost ot -- TODO: PositiveInt
  LoyaltyCost :: Loyalty -> Cost OTPlaneswalker
  ManaCost :: ManaCost 'Var -> Cost ot
  OrCosts :: [Cost ot] -> Cost ot
  PayLife :: Int -> Cost ot -- TODO: PositiveInt
  SacrificeCost :: IsZO 'ZBattlefield ot' => WPermanent ot' -> [Requirement 'ZBattlefield ot'] -> Cost ot
  TapCost :: IsZO 'ZBattlefield ot' => WPermanent ot' -> [Requirement 'ZBattlefield ot'] -> Cost ot
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

----------------------------------------

data Effect (ef :: EffectType) :: Type where
  AddMana :: OPlayer -> ManaPool 'NonSnow -> Effect 'OneShot -- NB: Engine will reinterpret as Snow when source is Snow.
  AddToBattlefield :: IsOT ot => WPermanent ot -> OPlayer -> Token ot -> Effect 'OneShot
  CantBeRegenerated :: OCreature -> Effect 'Continuous
  ChangeTo :: IsOT ot => WPermanent ot -> OPermanent -> Card ot -> Effect 'Continuous
  CounterAbility :: ZO 'ZStack OTActivatedOrTriggeredAbility -> Effect 'OneShot
  CounterSpell :: ZO 'ZStack OTSpell -> Effect 'OneShot
  DealDamage :: ODamageSource -> OCreaturePlayerPlaneswalker -> Damage 'Var -> Effect 'OneShot
  Destroy :: OPermanent -> Effect 'OneShot
  DrawCards :: OPlayer -> Int -> Effect 'OneShot
  EffectCase :: Case (Effect ef) -> Effect ef
  EffectContinuous :: Effect 'Continuous -> Effect 'OneShot -- 611.2
  Gain :: IsOT ot => WAny ot -> ZO 'ZBattlefield ot -> Ability ot -> Effect 'Continuous
  Lose :: IsOT ot => WAny ot -> ZO 'ZBattlefield ot -> Ability ot -> Effect 'Continuous
  PutOntoBattlefield :: IsZO zone ot => WPermanent ot -> OPlayer -> ZO zone ot -> Effect 'OneShot -- TODO: zone /= 'ZBattlefield
  Sacrifice :: IsOT ot => WPermanent ot -> OPlayer -> [Requirement 'ZBattlefield ot] -> Effect 'OneShot
  SearchLibrary :: IsOT ot => WCard ot -> OPlayer -> WithLinkedObject 'ZLibrary (Elect 'Post (Effect 'OneShot)) ot -> Effect 'OneShot
  Sequence :: [Effect ef] -> Effect ef
  StatDelta :: OCreature -> Power -> Toughness -> Effect 'Continuous
  Until :: Elect 'Post Event OTPlayer -> Effect 'Continuous -> Effect 'Continuous
  deriving (Typeable)

instance ConsIndex (Effect ef) where
  consIndex = \case
    AddMana{} -> 1
    AddToBattlefield{} -> 2
    CantBeRegenerated{} -> 3
    ChangeTo{} -> 4
    CounterAbility{} -> 5
    CounterSpell{} -> 6
    DealDamage{} -> 7
    Destroy{} -> 8
    EffectCase{} -> 9
    EffectContinuous{} -> 10
    DrawCards{} -> 11
    Gain{} -> 12
    Lose{} -> 13
    PutOntoBattlefield{} -> 14
    Sacrifice{} -> 15
    SearchLibrary{} -> 16
    Sequence{} -> 17
    StatDelta{} -> 18
    Until{} -> 19

----------------------------------------

data Elect (p :: PrePost) (el :: Type) (ot :: Type) :: Type where
  ActivePlayer :: (OPlayer -> Elect p el ot) -> Elect p el ot
  -- TODO: Add `IsZO zone ot` witness and change `'ZBattlefield` to `zone`.
  All :: IsOT ot => WithMaskedObject 'ZBattlefield (Elect 'Post el ot) -> Elect 'Post el ot
  CardTypeDef :: IsTribal tribal => CardTypeDef tribal ot -> Elect 'Pre (CardTypeDef tribal ot) ot
  -- TODO: Disallow `Choose` for some types of `el` using a witness arg, in particular Event and EventListener
  ElectCase :: Case (Elect p el ot) -> Elect p el ot
  Choose ::
    (IsPrePost p, Typeable el, IsZO zone ot) =>
    OPlayer ->
    WithMaskedObject zone (Elect p el ot) ->
    Elect p el ot
  ChooseColor ::
    (IsPrePost p, Typeable el, IsOT ot) =>
    OPlayer ->
    [Color] ->
    (Variable Color -> Elect p el ot) ->
    Elect p el ot -- TODO: Non-empty list
  Condition :: Condition -> Elect p Condition ot
  ControllerOf :: IsZO zone OTAny => ZO zone OTAny -> (OPlayer -> Elect p el ot) -> Elect p el ot
  Cost :: Cost ot -> Elect 'Post (Cost ot) ot
  Effect :: [Effect ef] -> Elect 'Post (Effect ef) ot
  Elect :: Elect 'Post el ot -> ElectPrePost el ot
  Event :: Event -> Elect 'Post Event ot
  If :: Condition -> Elect p el ot -> Else p el ot -> Elect p el ot -- NB: It is probably correct to allow this constructor with CardTypeDef usage in order to encode split cards and such.
  Listen :: EventListener -> Elect 'Post EventListener ot
  -- TODO: Add `IsZO zone ot` witness and change `'ZBattlefield` to `zone`.
  -- TODO: Prolly allow both 'Pre and 'Post
  -- XXX: `Random` is potentially problematic when done within an aggrogate Elect such as `All`...
  --    Solutions:
  --      (1) Use 'Pre instead of 'Post, but I think 'Post effects will need this
  --      (2) Introduce 'Mid for between 'Pre and 'Post to enforce it happens before aggrogates
  --      (3) Say that this is OK and say that Random must come before All if want it unified. Seems good actually...
  Random ::
    IsOT ot =>
    WithMaskedObject 'ZBattlefield (Elect 'Post el ot) ->
    Elect 'Post el ot -- Interpreted as "Arbitrary" in some contexts, such as Event and EventListener

  -- TODO: Disallow `Target` for some types of `el` using a witness arg, in particular Event and EventListener
  Target ::
    (Typeable el, IsZO zone ot) =>
    OPlayer ->
    WithMaskedObject zone (Elect 'Pre el ot) ->
    Elect 'Pre el ot
  VariableFromPower :: OCreature -> (Variable Int -> Elect 'Post el ot) -> Elect 'Post el ot
  deriving (Typeable)

instance ConsIndex (Elect p el ot) where
  consIndex = \case
    ActivePlayer{} -> 1
    All{} -> 2
    CardTypeDef{} -> 3
    Choose{} -> 4
    ChooseColor{} -> 5
    Condition{} -> 6
    ControllerOf{} -> 7
    Cost{} -> 8
    Effect{} -> 9
    Elect{} -> 10
    ElectCase{} -> 11
    Event{} -> 12
    If{} -> 13
    Listen{} -> 14
    Random{} -> 15
    Target{} -> 16
    VariableFromPower{} -> 17

type ElectPrePost el ot = Elect 'Pre (Elect 'Post el ot) ot

----------------------------------------

data Else (p :: PrePost) (el :: Type) (ot :: Type) :: Type where
  ElseCost :: (el ~ Cost ot) => Elect p el ot -> Else p el ot
  ElseEffect :: (el ~ Effect 'OneShot) => Elect p el ot -> Else p el ot
  -- NB: Events need linear history to make sense of election costs tied to it, hence this hole.
  -- Imagine otherwise this were not the case. Then different parts of the branch could listen to different
  -- event types (without injecting yet another index/witness to prevent it). This is is dumb on its own
  -- and gets worse when the conditional has costs involved. You'd have to solve for the future to know what
  -- costs are paid in order to know which event to trigger! Impossible!
  ElseEvent :: (el ~ EventListener' liftOT) => Else p el ot
  deriving (Typeable)

instance ConsIndex (Else p el ot) where
  consIndex = \case
    ElseCost{} -> 1
    ElseEffect{} -> 2
    ElseEvent{} -> 3

type ElsePrePost el ot = Else 'Pre (Else 'Post el ot) ot

----------------------------------------

data Enchant (zone :: Zone) (ot :: Type) :: Type where
  Enchant :: IsZO zone ot => WithLinkedObject zone (Elect 'Pre (Elect 'Post (Effect 'Continuous) ot)) ot -> Enchant zone ot
  deriving (Typeable)

instance ConsIndex (Enchant zone ot) where
  consIndex = \case
    Enchant{} -> 1

----------------------------------------

data EnchantmentType (ot :: Type) :: Type where
  Aura :: (ot ~ OTEnchantment, IsZO zone ot') => Enchant zone ot' -> EnchantmentType ot
  deriving (Typeable)

instance ConsIndex (EnchantmentType ot) where
  consIndex = \case
    Aura{} -> 1

----------------------------------------

type Event = EventListener' Proxy

type EventListener = EventListener' (Elect 'Post (Effect 'OneShot))

data EventListener' (liftOT :: Type -> Type) :: Type where
  BecomesTapped :: (IsOT ot, Typeable liftOT) => WPermanent ot -> WithLinkedObject 'ZBattlefield liftOT ot -> EventListener' liftOT
  Events :: [EventListener' liftOT] -> EventListener' liftOT
  SpellIsCast :: IsOT ot => WSpell ot -> WithLinkedObject 'ZBattlefield liftOT ot -> EventListener' liftOT
  TimePoint :: Typeable p => TimePoint p -> liftOT OTPlayer -> EventListener' liftOT
  deriving (Typeable)

instance ConsIndex (EventListener' liftOT) where
  consIndex = \case
    BecomesTapped{} -> 1
    Events{} -> 2
    SpellIsCast{} -> 3
    TimePoint{} -> 4

----------------------------------------

data NonProxy (liftOT :: Type -> Type) :: Type where
  NonProxyElectEffect :: NonProxy (Elect p (Effect ef))
  NonProxyElectPrePostEffect :: NonProxy (Elect 'Pre (Elect 'Post (Effect 'Continuous) ot))
  deriving (Typeable)

instance ConsIndex (NonProxy liftOT) where
  consIndex = \case
    NonProxyElectEffect -> 1
    NonProxyElectPrePostEffect -> 2

----------------------------------------

data Requirement (zone :: Zone) (ot :: Type) :: Type where
  ControlledBy :: IsOT ot => OPlayer -> Requirement 'ZBattlefield ot
  ControlsA :: IsOT ot => Requirement 'ZBattlefield ot -> Requirement zone OTPlayer
  HasAbility :: IsZO zone ot => WithThis zone Ability ot -> Requirement zone ot -- Non-unique differing representations will not be considered the same
  HasLandType :: IsZO zone OTLand => LandType -> Requirement zone OTLand
  Is :: IsZO zone ot => WAny ot -> ZO zone ot -> Requirement zone ot
  IsTapped :: IsOT ot => WPermanent ot -> Requirement 'ZBattlefield ot
  Not :: IsZO zone ot => Requirement zone ot -> Requirement zone ot
  OfColors :: IsZO zone ot => Colors -> Requirement zone ot -- needs `WCard a` witness
  OwnedBy :: IsZO zone ot => OPlayer -> Requirement zone ot
  PlayerPays :: IsZO zone OTPlayer => Cost OPlayer -> Requirement zone OTPlayer
  RAnd :: IsZO zone ot => [Requirement zone ot] -> Requirement zone ot
  ROr :: IsZO zone ot => [Requirement zone ot] -> Requirement zone ot
  -- TODO: Try to add some combinators that go from: forall a b. [forall liftOT. Requirement x] -> Requirement (ON2 a, b)
  R2 ::
    ( IsZO zone (OT2 a b)
    , Inst2 IsObjectType a b
    ) =>
    [Requirement zone (OT1 a)] ->
    [Requirement zone (OT1 b)] ->
    Requirement zone (OT2 a b)
  R3 ::
    ( IsZO zone (OT3 a b c)
    , Inst3 IsObjectType a b c
    ) =>
    [Requirement zone (OT1 a)] ->
    [Requirement zone (OT1 b)] ->
    [Requirement zone (OT1 c)] ->
    Requirement zone (OT3 a b c)
  R4 ::
    ( IsZO zone (OT4 a b c d)
    , Inst4 IsObjectType a b c d
    ) =>
    [Requirement zone (OT1 a)] ->
    [Requirement zone (OT1 b)] ->
    [Requirement zone (OT1 c)] ->
    [Requirement zone (OT1 d)] ->
    Requirement zone (OT4 a b c d)
  R5 ::
    ( IsZO zone (OT5 a b c d e)
    , Inst5 IsObjectType a b c d e
    ) =>
    [Requirement zone (OT1 a)] ->
    [Requirement zone (OT1 b)] ->
    [Requirement zone (OT1 c)] ->
    [Requirement zone (OT1 d)] ->
    [Requirement zone (OT1 e)] ->
    Requirement zone (OT5 a b c d e)
  deriving (Typeable)

instance ConsIndex (Requirement zone ot) where
  consIndex = \case
    ControlsA{} -> 1
    ControlledBy{} -> 2
    HasAbility{} -> 3
    HasLandType{} -> 4
    Is{} -> 5
    IsTapped{} -> 6
    Not{} -> 7
    OfColors{} -> 8
    OwnedBy{} -> 9
    PlayerPays{} -> 10
    RAnd{} -> 11
    ROr{} -> 12
    R2{} -> 13
    R3{} -> 14
    R4{} -> 15
    R5{} -> 16

----------------------------------------

-- XXX: Better to just make this take a user type `a` which can encode CardSet and Rarirty amongst other things.
data SetCard (ot :: Type) :: Type where
  SetCard :: CardSet -> Rarity -> Card ot -> SetCard ot
  deriving (Typeable)

instance ConsIndex (SetCard ot) where
  consIndex = \case
    SetCard{} -> 1

----------------------------------------

-- XXX: Better to just make this take a user type `a` which can encode CardSet and Rarirty amongst other things.
data SetToken (ot :: Type) :: Type where
  SetToken :: CardSet -> Rarity -> Token ot -> SetToken ot
  deriving (Typeable)

instance ConsIndex (SetToken ot) where
  consIndex = \case
    SetToken{} -> 1

----------------------------------------

-- TODO: Move all these Some* stuff to another file

data Some (liftOT :: Type -> Type) (ot :: Type) :: Type where
  Some2a :: Inst2 IsObjectType a b => SomeTerm liftOT (OT1 a) -> Some liftOT (OT2 a b)
  Some2b :: Inst2 IsObjectType a b => SomeTerm liftOT (OT1 b) -> Some liftOT (OT2 a b)
  --
  Some5a :: Inst5 IsObjectType a b c d e => SomeTerm liftOT (OT1 a) -> Some liftOT (OT5 a b c d e)
  Some5b :: Inst5 IsObjectType a b c d e => SomeTerm liftOT (OT1 b) -> Some liftOT (OT5 a b c d e)
  Some5c :: Inst5 IsObjectType a b c d e => SomeTerm liftOT (OT1 c) -> Some liftOT (OT5 a b c d e)
  Some5d :: Inst5 IsObjectType a b c d e => SomeTerm liftOT (OT1 d) -> Some liftOT (OT5 a b c d e)
  Some5e :: Inst5 IsObjectType a b c d e => SomeTerm liftOT (OT1 e) -> Some liftOT (OT5 a b c d e)
  Some5ab :: Inst5 IsObjectType a b c d e => SomeTerm liftOT (OT2 a b) -> Some liftOT (OT5 a b c d e)
  Some5ad :: Inst5 IsObjectType a b c d e => SomeTerm liftOT (OT2 a d) -> Some liftOT (OT5 a b c d e)
  Some5bc :: Inst5 IsObjectType a b c d e => SomeTerm liftOT (OT2 b c) -> Some liftOT (OT5 a b c d e)
  --
  Some6a :: Inst6 IsObjectType a b c d e f => SomeTerm liftOT (OT1 a) -> Some liftOT (OT6 a b c d e f)
  Some6b :: Inst6 IsObjectType a b c d e f => SomeTerm liftOT (OT1 b) -> Some liftOT (OT6 a b c d e f)
  Some6c :: Inst6 IsObjectType a b c d e f => SomeTerm liftOT (OT1 c) -> Some liftOT (OT6 a b c d e f)
  Some6d :: Inst6 IsObjectType a b c d e f => SomeTerm liftOT (OT1 d) -> Some liftOT (OT6 a b c d e f)
  Some6e :: Inst6 IsObjectType a b c d e f => SomeTerm liftOT (OT1 e) -> Some liftOT (OT6 a b c d e f)
  Some6f :: Inst6 IsObjectType a b c d e f => SomeTerm liftOT (OT1 f) -> Some liftOT (OT6 a b c d e f)
  Some6ab :: Inst6 IsObjectType a b c d e f => SomeTerm liftOT (OT2 a b) -> Some liftOT (OT6 a b c d e f)
  Some6bc :: Inst6 IsObjectType a b c d e f => SomeTerm liftOT (OT2 b c) -> Some liftOT (OT6 a b c d e f)
  -- TODO: Write a script to generate other Some6xy flavors
  deriving (Typeable)

data SomeTerm (liftOT :: Type -> Type) (ot :: Type) :: Type where
  SomeArtifact :: liftOT OTArtifact -> SomeTerm liftOT OTArtifact
  SomeCreature :: liftOT OTCreature -> SomeTerm liftOT OTCreature
  SomeEnchantment :: liftOT OTEnchantment -> SomeTerm liftOT OTEnchantment
  SomeInstant :: liftOT OTInstant -> SomeTerm liftOT OTInstant
  SomeLand :: liftOT OTLand -> SomeTerm liftOT OTLand
  SomePlaneswalker :: liftOT OTPlaneswalker -> SomeTerm liftOT OTPlaneswalker
  SomeSorcery :: liftOT OTSorcery -> SomeTerm liftOT OTSorcery
  SomeArtifactCreature :: liftOT OTArtifactCreature -> SomeTerm liftOT OTArtifactCreature
  SomeArtifactLand :: liftOT OTArtifactLand -> SomeTerm liftOT OTArtifactLand
  SomeEnchantmentCreature :: liftOT OTEnchantmentCreature -> SomeTerm liftOT OTEnchantmentCreature
  deriving (Typeable)

type SomeCard = Some Card

type SomeToken = Some Token

type SomeCardOrToken ot = Either (SomeCard ot) (SomeToken ot)

----------------------------------------

data StaticAbility (zone :: Zone) (ot :: Type) :: Type where
  As :: IsOT ot => Elect 'Post EventListener ot -> StaticAbility 'ZBattlefield ot -- 603.6d: not a triggered ability
  Bestow :: ot ~ OTEnchantmentCreature => ElectPrePost (Cost ot) ot -> Enchant 'ZBattlefield OTCreature -> StaticAbility 'ZBattlefield ot
  FirstStrike :: ot ~ OTCreature => StaticAbility 'ZBattlefield ot
  Flying :: ot ~ OTCreature => StaticAbility 'ZBattlefield ot
  Haste :: ot ~ OTCreature => StaticAbility 'ZBattlefield ot
  StaticContinuous :: IsOT ot => Elect 'Post (Effect 'Continuous) ot -> StaticAbility 'ZBattlefield ot -- 611.3
  Suspend :: IsOT ot => Int -> ElectPrePost (Cost ot) ot -> StaticAbility 'ZBattlefield ot -- PositiveInt
  deriving (Typeable)

instance ConsIndex (StaticAbility zone ot) where
  consIndex = \case
    As{} -> 1
    Bestow{} -> 2
    FirstStrike{} -> 3
    Flying{} -> 4
    Haste{} -> 5
    StaticContinuous{} -> 6
    Suspend{} -> 7

----------------------------------------

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

----------------------------------------

-- https://www.mtgsalvation.com/forums/magic-fundamentals/magic-rulings/magic-rulings-archives/611601-whenever-what-does-it-mean?comment=3
-- https://www.reddit.com/r/magicTCG/comments/asmecb/noob_question_difference_between_as_and_when/
data TriggeredAbility (zone :: Zone) (ot :: Type) :: Type where
  When :: IsOT ot => Elect 'Post EventListener ot -> TriggeredAbility 'ZBattlefield ot
  deriving (Typeable)

instance ConsIndex (TriggeredAbility zone ot) where
  consIndex = \case
    When{} -> 1

----------------------------------------

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

----------------------------------------

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
  M6 ::
    (Typeable liftedOT, IsOT (OT6 a b c d e f), Inst6 IsObjectType a b c d e f) =>
    [Requirement zone (OT6 a b c d e f)] ->
    (ZO zone (OT6 a b c d e f) -> liftedOT) ->
    WithMaskedObject zone liftedOT
  deriving (Typeable)

instance ConsIndex (WithMaskedObject zone liftedOT) where
  consIndex = \case
    M1{} -> 1
    M2{} -> 2
    M3{} -> 3
    M4{} -> 4
    M5{} -> 5
    M6{} -> 6

----------------------------------------

data WithThis (zone :: Zone) (liftOT :: Type -> Type) (ot :: Type) :: Type where
  T1 ::
    (IsOT (OT1 a), Inst1 IsObjectType a) =>
    (ZO zone (OT1 a) -> liftOT (OT1 a)) ->
    WithThis zone liftOT (OT1 a)
  T2 ::
    (IsOT (OT2 a b), Inst2 IsObjectType a b) =>
    -- TODO: Add a additional full (ZO zone (OT2 a b)) to the input tuple?
    -- TODO: Introduce a `This ot` record type to access its constituents.
    --       Prolly can also add ToObjectN instances (cool!).
    ((ZO zone (OT1 a), ZO zone (OT1 b)) -> liftOT (OT2 a b)) ->
    WithThis zone liftOT (OT2 a b)
  deriving (Typeable)

instance ConsIndex (WithThis zone liftOT ot) where
  consIndex = \case
    T1{} -> 1
    T2{} -> 2
