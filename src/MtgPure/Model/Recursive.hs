{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module MtgPure.Model.Recursive (
  Ability (..),
  ActivatedAbility (..),
  AnyCard (..),
  AnyToken (..),
  Card (..),
  CardFacet (..),
  CardFacet' (..),
  Case (..),
  Condition (..),
  Cost (..),
  Effect (..),
  Elect (..),
  ElectOT (..),
  ElectTargetedEffect,
  Else (..),
  Enchant (..),
  EnchantmentType (..),
  EntersStatic (..),
  Event,
  EventListener,
  EventListener' (..),
  IsSpecificCard (..),
  IsUser (..),
  List (..),
  NonProxy (..),
  Requirement (..),
  SetCard (..),
  SetToken (..),
  SomeOT (..),
  SomeCard (..),
  SomeCardOrToken,
  SomeTerm (..),
  SomeToken,
  SpecificCard (..),
  StaticAbility (..),
  Token (..),
  TriggeredAbility (..),
  WithLinkedObject (..),
  WithList (..),
  WithMaskedObject (..),
  WithMaskedObjects (..),
  WithThis (..),
  WithThisAbility (..),
  WithThisActivated,
  WithThisOneShot,
  WithThisStatic,
  WithThisTriggered,
  WithThisZ (..),
  SomeZone (..),
  pattern CFalse,
  pattern CTrue,
  fromSomeOT,
  mapSomeOT,
) where

import safe Data.ConsIndex (ConsIndex (..))
import safe Data.Inst (Inst1, Inst2, Inst3, Inst4, Inst5, Inst6)
import safe Data.Kind (Type)
import safe Data.Nat (Fin, IsNat, NatList)
import safe Data.Proxy (Proxy (..))
import safe Data.Typeable (Typeable, typeRep)
import safe MtgPure.Model.ArtifactType (ArtifactType)
import safe MtgPure.Model.CardName (CardName, HasCardName (..))
import safe MtgPure.Model.CardSet (CardSet)
import safe MtgPure.Model.Color (Color)
import safe MtgPure.Model.Colors (Colors)
import safe MtgPure.Model.CreatureType (CreatureType)
import safe MtgPure.Model.Damage (Damage)
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.ElectStage (CoNonIntrinsicStage, ElectStage (..))
import safe MtgPure.Model.LandType (LandType)
import safe MtgPure.Model.Loyalty (Loyalty)
import safe MtgPure.Model.Mana.ManaCost (ManaCost)
import safe MtgPure.Model.Mana.ManaPool (ManaPool)
import safe MtgPure.Model.Mana.Snow (Snow (..))
import safe MtgPure.Model.Object.IsObjectType (IsObjectType)
import safe MtgPure.Model.Object.OTN (OT1, OT2, OT3, OT4, OT5, OT6, OTN)
import safe MtgPure.Model.Object.OTNAliases (
  OTNActivatedOrTriggeredAbility,
  OTNAny,
  OTNArtifact,
  OTNArtifactCreature,
  OTNArtifactLand,
  OTNCreature,
  OTNDamageSource,
  OTNEnchantment,
  OTNEnchantmentCreature,
  OTNInstant,
  OTNLand,
  OTNPlaneswalker,
  OTNPlayer,
  OTNSorcery,
  OTNSpell,
 )
import safe MtgPure.Model.Object.Singleton.Any (CoAny)
import safe MtgPure.Model.Object.Singleton.Card (CoCard)
import safe MtgPure.Model.Object.Singleton.Permanent (CoPermanent)
import safe MtgPure.Model.Object.Singleton.Spell (CoSpell)
import safe MtgPure.Model.Power (Power)
import safe MtgPure.Model.Rarity (Rarity)
import safe MtgPure.Model.Supertype (Supertype)
import safe MtgPure.Model.TimePoint (TimePoint)
import safe MtgPure.Model.Toughness (Toughness)
import safe MtgPure.Model.Variable (Var (Var), Variable)
import safe MtgPure.Model.Zone (IsZone, Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (
  IsOTN,
  IsZO,
  ZO,
  ZOCreature,
  ZOCreaturePlayerPlaneswalker,
  ZOPermanent,
  ZOPlayer,
 )

--------------------------------------------------------------------------------

-- Semantics legend:
--
-- "ot is exactly (a,b,c,...)"
--    Ability ot (experimenting with engine not having this "exact" notion)
--    Card ot
--    EnchantmentType ot
--    WithThis liftOT zone ot
--
-- "ot is (at least) one of (a,b,c,...)"
--    Enchant zone ot
--    Requirement zone ot
--    SomeOT liftOT ot
--    ZO zone ot

--------------------------------------------------------------------------------

data Ability (zone :: Zone) (ot :: Type) :: Type where
  Activated :: IsZO zone ot => Elect 'IntrinsicStage (ActivatedAbility zone ot) ot -> Ability zone ot
  Static :: StaticAbility zone ot -> Ability zone ot
  Triggered :: IsZO zone ot => TriggeredAbility zone ot -> Ability zone ot
  deriving (Typeable)

instance ConsIndex (Ability zone ot) where
  consIndex = \case
    Activated{} -> 1
    Static{} -> 2
    Triggered{} -> 3

--------------------------------------------------------------------------------

data ActivatedAbility (zone :: Zone) (ot :: Type) :: Type where
  Ability ::
    IsZO zone ot =>
    { activated_cost :: Cost ot
    , activated_effect :: Elect 'ResolveStage (Effect 'OneShot) ot
    } ->
    ActivatedAbility zone ot
  Cycling :: (ot ~ OTN x, IsOTN ot) => Cost ot -> ActivatedAbility 'ZHand ot
  deriving (Typeable)

instance ConsIndex (ActivatedAbility zone ot) where
  consIndex = \case
    Ability{} -> 1
    Cycling{} -> 2

--------------------------------------------------------------------------------

-- data ArtifactType (ot :: Type) :: Type where
--   Vehicle :: ArtifactType ot -- TODO: Stuff crew mechanic into here
--   deriving (Bounded, Enum, Eq, Ord, Show)

--------------------------------------------------------------------------------

-- TODO: Move out of Recursive.hs
class Typeable (u :: Type) => IsUser u where
  showUserType :: String
  showUserType = show (typeRep (Proxy @u))

instance IsUser () where
  showUserType = "()"

instance IsUser Color

-- TODO: Move out of Recursive.hs
-- TODO: EnchantmentLand [Urza's Saga]
-- TODO: EnchantmentArtifactCreature [Hammer of Purphoros]
-- TODO: LandCreature [Dryad Arbor]
data SpecificCard (ot :: Type) :: Type where
  ArtifactCard :: OTNArtifact ~ ot => SpecificCard ot
  ArtifactCreatureCard :: OTNArtifactCreature ~ ot => SpecificCard ot
  ArtifactLandCard :: OTNArtifactLand ~ ot => SpecificCard ot
  CreatureCard :: OTNCreature ~ ot => SpecificCard ot
  EnchantmentCard :: OTNEnchantment ~ ot => SpecificCard ot
  EnchantmentCreatureCard :: OTNEnchantmentCreature ~ ot => SpecificCard ot
  InstantCard :: OTNInstant ~ ot => SpecificCard ot
  LandCard :: OTNLand ~ ot => SpecificCard ot
  PlaneswalkerCard :: OTNPlaneswalker ~ ot => SpecificCard ot
  SorceryCard :: OTNSorcery ~ ot => SpecificCard ot
  deriving (Typeable)

instance ConsIndex (SpecificCard ot) where
  consIndex = \case
    ArtifactCard{} -> 1
    ArtifactCreatureCard{} -> 2
    ArtifactLandCard{} -> 3
    CreatureCard{} -> 4
    EnchantmentCard{} -> 5
    EnchantmentCreatureCard{} -> 6
    InstantCard{} -> 7
    LandCard{} -> 8
    PlaneswalkerCard{} -> 9
    SorceryCard{} -> 10

class IsOTN ot => IsSpecificCard (ot :: Type) where
  singSpecificCard :: SpecificCard ot

instance IsSpecificCard OTNArtifact where
  singSpecificCard = ArtifactCard

instance IsSpecificCard OTNArtifactCreature where
  singSpecificCard = ArtifactCreatureCard

instance IsSpecificCard OTNArtifactLand where
  singSpecificCard = ArtifactLandCard

instance IsSpecificCard OTNCreature where
  singSpecificCard = CreatureCard

instance IsSpecificCard OTNEnchantment where
  singSpecificCard = EnchantmentCard

instance IsSpecificCard OTNEnchantmentCreature where
  singSpecificCard = EnchantmentCreatureCard

instance IsSpecificCard OTNInstant where
  singSpecificCard = InstantCard

instance IsSpecificCard OTNLand where
  singSpecificCard = LandCard

instance IsSpecificCard OTNPlaneswalker where
  singSpecificCard = PlaneswalkerCard

instance IsSpecificCard OTNSorcery where
  singSpecificCard = SorceryCard

--------------------------------------------------------------------------------

data AnyCard :: Type where
  AnyCard1 :: (ot ~ OTN x, IsSpecificCard ot) => Card ot -> AnyCard
  AnyCard2 :: (IsSpecificCard ot1, IsSpecificCard ot2) => Card (ot1, ot2) -> AnyCard
  deriving (Typeable)

instance ConsIndex AnyCard where
  consIndex = \case
    AnyCard1{} -> 1
    AnyCard2{} -> 2

instance HasCardName AnyCard where
  getCardName = \case
    AnyCard1 card -> getCardName card
    AnyCard2 card -> getCardName card

--------------------------------------------------------------------------------

data AnyToken :: Type where
  AnyToken :: IsSpecificCard ot => Token ot -> AnyToken
  deriving (Typeable)

instance ConsIndex AnyToken where
  consIndex = \case
    AnyToken{} -> 1

instance HasCardName AnyToken where
  getCardName = \case
    AnyToken token -> getCardName token

--------------------------------------------------------------------------------

data Card (ot :: Type) :: Type where
  Card :: (ot ~ OTN x, IsSpecificCard ot) => CardName -> Elect 'IntrinsicStage (CardFacet ot) ot -> Card ot
  DoubleSidedCard :: (ot1 ~ OTN x, ot2 ~ OTN y, Inst2 IsSpecificCard ot1 ot2) => Card ot1 -> Card ot2 -> Card (ot1, ot2)
  SplitCard ::
    (ot1 ~ OTN x, ot2 ~ OTN y, Inst2 IsSpecificCard ot1 ot2) =>
    { splitCard_card1 :: Card ot1
    , splitCard_card2 :: Card ot2
    , splitCard_abilities :: [SomeZone Ability (ot1, ot2)]
    } ->
    Card (ot1, ot2)
  deriving (Typeable)

instance ConsIndex (Card ot) where
  consIndex = \case
    Card{} -> 1
    DoubleSidedCard{} -> 2
    SplitCard{} -> 3

instance HasCardName (Card ot) where
  getCardName = \case
    Card name _ -> name
    DoubleSidedCard card1 card2 -> getCardName card1 <> " // " <> getCardName card2
    SplitCard card1 card2 _ -> getCardName card1 <> " // " <> getCardName card2

--------------------------------------------------------------------------------

data CardFacet (ot :: Type) :: Type where
  ArtifactFacet ::
    { artifact_colors :: Colors
    , artifact_supertypes :: [Supertype OTNArtifact]
    , artifact_artifactTypes :: [ArtifactType]
    , artifact_spec :: CardFacet' OTNArtifact
    } ->
    CardFacet OTNArtifact
  ArtifactCreatureFacet ::
    { artifactCreature_colors :: Colors
    , artifactCreature_supertypes :: [Supertype OTNArtifactCreature]
    , artifactCreature_artifactTypes :: [ArtifactType]
    , artifactCreature_creatureTypes :: [CreatureType] -- TODO: Non-empty
    , artifactCreature_spec :: CardFacet' OTNArtifactCreature
    } ->
    CardFacet OTNArtifactCreature
  ArtifactLandFacet ::
    { artifactLand_supertypes :: [Supertype OTNArtifactLand]
    , artifactLand_artifactTypes :: [ArtifactType]
    , artifactLand_landTypes :: [LandType]
    , artifactLand_spec :: CardFacet' OTNArtifactLand
    } ->
    CardFacet OTNArtifactLand
  CreatureFacet ::
    { creature_colors :: Colors
    , creature_supertypes :: [Supertype OTNCreature]
    , creature_creatureTypes :: [CreatureType] -- TODO: Non-empty
    , creature_spec :: CardFacet' OTNCreature
    } ->
    CardFacet OTNCreature
  EnchantmentFacet ::
    { enchantment_colors :: Colors
    , enchantment_supertypes :: [Supertype OTNEnchantment]
    , enchantment_enchantmentTypes :: [EnchantmentType OTNEnchantment]
    , enchantment_spec :: CardFacet' OTNEnchantment
    } ->
    CardFacet OTNEnchantment
  EnchantmentCreatureFacet ::
    { enchantmentCreature_colors :: Colors
    , enchantmentCreature_supertypes :: [Supertype OTNEnchantmentCreature]
    , enchantmentCreature_creatureTypes :: [CreatureType] -- TODO: Non-empty
    , enchantmentCreature_enchantmentTypes :: [EnchantmentType OTNEnchantmentCreature]
    , enchantmentCreature_spec :: CardFacet' OTNEnchantmentCreature
    } ->
    CardFacet OTNEnchantmentCreature
  InstantFacet ::
    { instant_colors :: Colors
    , instant_supertypes :: [Supertype OTNInstant]
    , instant_spec :: Elect 'TargetStage (CardFacet' OTNInstant) OTNInstant
    } ->
    CardFacet OTNInstant
  LandFacet ::
    { land_supertypes :: [Supertype OTNLand]
    , land_landTypes :: [LandType]
    , land_spec :: CardFacet' OTNLand
    } ->
    CardFacet OTNLand
  PlaneswalkerFacet ::
    { planeswalker_colors :: Colors
    , planeswalker_supertypes :: [Supertype OTNPlaneswalker]
    , planeswalker_spec :: CardFacet' OTNPlaneswalker
    } ->
    CardFacet OTNPlaneswalker
  SorceryFacet ::
    { sorcery_colors :: Colors
    , sorcery_supertypes :: [Supertype OTNSorcery]
    , sorcery_spec :: Elect 'TargetStage (CardFacet' OTNSorcery) OTNSorcery
    } ->
    CardFacet OTNSorcery
  deriving (Typeable)

instance ConsIndex (CardFacet ot) where
  consIndex = \case
    ArtifactFacet{} -> 1
    ArtifactCreatureFacet{} -> 2
    ArtifactLandFacet{} -> 3
    CreatureFacet{} -> 4
    EnchantmentCreatureFacet{} -> 5
    EnchantmentFacet{} -> 6
    InstantFacet{} -> 7
    LandFacet{} -> 8
    PlaneswalkerFacet{} -> 9
    SorceryFacet{} -> 10

--------------------------------------------------------------------------------

data CardFacet' (ot :: Type) :: Type where
  ArtifactFacet' ::
    { artifact_cost :: Cost OTNArtifact
    , artifact_abilities :: [SomeZone WithThisAbility OTNArtifact]
    } ->
    CardFacet' OTNArtifact
  ArtifactCreatureFacet' ::
    { artifactCreature_cost :: Cost OTNArtifactCreature
    , artifactCreature_power :: Power
    , artifactCreature_toughness :: Toughness
    , -- TODO: artifactCreature_abilities :: [SomeOT (SomeZone WithThisAbility) OTNArtifactCreature]
      artifactCreature_artifactAbilities :: [SomeZone WithThisAbility OTNArtifact]
    , artifactCreature_creatureAbilities :: [SomeZone WithThisAbility OTNCreature]
    , artifactCreature_artifactCreatureAbilities :: [SomeZone WithThisAbility OTNArtifactCreature]
    } ->
    CardFacet' OTNArtifactCreature
  ArtifactLandFacet' ::
    { artifactLand_artifactAbilities :: [SomeZone WithThisAbility OTNArtifact]
    , artifactLand_landAbilities :: [SomeZone WithThisAbility OTNLand]
    , artifactLand_artifactLandAbilities :: [SomeZone WithThisAbility OTNArtifactLand]
    } ->
    CardFacet' OTNArtifactLand
  CreatureFacet' ::
    { creature_cost :: Cost OTNCreature
    , creature_power :: Power
    , creature_toughness :: Toughness
    , creature_abilities :: [SomeZone WithThisAbility OTNCreature]
    } ->
    CardFacet' OTNCreature
  EnchantmentFacet' ::
    { enchantment_cost :: Cost OTNEnchantment
    , enchantment_abilities :: [SomeZone WithThisAbility OTNEnchantment]
    } ->
    CardFacet' OTNEnchantment
  EnchantmentCreatureFacet' ::
    { enchantmentCreature_cost :: Cost OTNEnchantmentCreature
    , enchantmentCreature_power :: Power
    , enchantmentCreature_toughness :: Toughness
    , enchantmentCreature_creatureAbilities :: [SomeZone WithThisAbility OTNCreature]
    , enchantmentCreature_enchantmentAbilities :: [SomeZone WithThisAbility OTNEnchantment]
    , enchantmentCreature_enchantmentCreatureAbilities :: [SomeZone WithThisAbility OTNEnchantmentCreature]
    } ->
    CardFacet' OTNEnchantmentCreature
  InstantFacet' ::
    { instant_cost :: Cost OTNInstant
    , instant_abilities :: [SomeZone WithThisAbility OTNInstant]
    , instant_effect :: WithThisOneShot OTNInstant
    } ->
    CardFacet' OTNInstant
  LandFacet' ::
    { land_abilities :: [SomeZone WithThisAbility OTNLand]
    } ->
    CardFacet' OTNLand
  PlaneswalkerFacet' ::
    { planeswalker_cost :: Cost OTNPlaneswalker
    , planeswalker_loyalty :: Loyalty
    , planeswalker_abilities :: [SomeZone WithThisAbility OTNPlaneswalker]
    } ->
    CardFacet' OTNPlaneswalker
  SorceryFacet' ::
    { sorcery_cost :: Cost OTNSorcery
    , sorcery_abilities :: [SomeZone WithThisAbility OTNSorcery]
    , sorcery_effect :: WithThisOneShot OTNSorcery
    } ->
    CardFacet' OTNSorcery
  deriving (Typeable)

instance ConsIndex (CardFacet' ot) where
  consIndex = \case
    ArtifactFacet'{} -> 1
    ArtifactCreatureFacet'{} -> 2
    ArtifactLandFacet'{} -> 3
    CreatureFacet'{} -> 4
    EnchantmentCreatureFacet'{} -> 5
    EnchantmentFacet'{} -> 6
    InstantFacet'{} -> 7
    LandFacet'{} -> 8
    PlaneswalkerFacet'{} -> 9
    SorceryFacet'{} -> 10

--------------------------------------------------------------------------------

data Case (x :: Type) where
  CaseFin ::
    (IsUser u, IsNat n) =>
    { caseFin :: Variable (Fin u n)
    , ofFin :: NatList u n x
    } ->
    Case x
  deriving (Typeable)

instance ConsIndex (Case x) where
  consIndex = \case
    CaseFin{} -> 1

--------------------------------------------------------------------------------

data Condition :: Type where
  CAnd :: [Condition] -> Condition
  CNot :: Condition -> Condition
  COr :: [Condition] -> Condition
  Satisfies :: (CoAny ot, IsZO zone ot) => ZO zone ot -> [Requirement zone ot] -> Condition
  deriving (Typeable)

instance ConsIndex Condition where
  consIndex = \case
    CAnd{} -> 1
    CNot{} -> 2
    COr{} -> 3
    Satisfies{} -> 4

pattern CFalse :: Condition
pattern CFalse = COr []

pattern CTrue :: Condition
pattern CTrue = CAnd []

--------------------------------------------------------------------------------

-- XXX: Uggh... need to add another type index for what to do after since some effects and abilities need to
-- know which costs were paid. (e.g. "If black mana was spend to pay this card's cost then ..."). Then a
-- continuation constructor needs to be provided. Then constructors of other types that use costs and something
-- else need to be updated accordingly. Ponder a less intrusive and less annoying solution... Perhaps Effect
-- (or whatever types) gets a constructor that can obtain an abstract runtime Cost which can be queried by
-- the API. This would likely require the model to encode more contingencies to handle dynamic issues, but this
-- is likely overwhelmingly worth it to avoid the continuation approach.
data Cost (ot :: Type) :: Type where
  AndCosts :: [Cost ot] -> Cost ot
  CostCase :: Case (Cost ot) -> Cost ot
  DiscardRandomCost :: Int -> Cost ot -- TODO: PositiveInt
  ExileCost :: IsZO zone ot' => [Requirement zone ot'] -> Cost ot -- TODO: prohibit (zone == 'ZExile)
  LoyaltyCost :: Loyalty -> Cost OTNPlaneswalker
  ManaCost :: ManaCost 'Var -> Cost ot
  OrCosts :: [Cost ot] -> Cost ot
  PayLife :: Int -> Cost ot -- TODO: PositiveInt
  SacrificeCost :: (CoPermanent ot', IsZO 'ZBattlefield ot') => [Requirement 'ZBattlefield ot'] -> Cost ot
  TapCost :: (CoPermanent ot', IsZO 'ZBattlefield ot') => [Requirement 'ZBattlefield ot'] -> Cost ot
  deriving (Typeable)

instance ConsIndex (Cost ot) where
  consIndex = \case
    AndCosts{} -> 1
    CostCase{} -> 2
    DiscardRandomCost{} -> 3
    ExileCost{} -> 4
    LoyaltyCost{} -> 5
    ManaCost{} -> 6
    OrCosts{} -> 7
    PayLife{} -> 8
    SacrificeCost{} -> 9
    TapCost{} -> 10

--------------------------------------------------------------------------------

data Effect (ef :: EffectType) :: Type where
  -- TODO: Need 'Var version of ManaPool for add X mana effects.
  AddMana :: ZOPlayer -> ManaPool 'NonSnow -> Effect 'OneShot -- NOTE: Engine will reinterpret as Snow when source is Snow.
  AddToBattlefield :: (CoPermanent ot, IsOTN ot) => ZOPlayer -> Token ot -> Effect 'OneShot
  CantBeRegenerated :: ZOCreature -> Effect 'Continuous
  ChangeTo :: (ot ~ OTN x, CoPermanent ot, IsOTN ot) => ZOPermanent -> Card ot -> Effect 'Continuous
  CounterAbility :: ZO 'ZStack OTNActivatedOrTriggeredAbility -> Effect 'OneShot
  CounterSpell :: ZO 'ZStack OTNSpell -> Effect 'OneShot
  DealDamage :: IsZO zone OTNDamageSource => ZO zone OTNDamageSource -> ZOCreaturePlayerPlaneswalker -> Damage 'Var -> Effect 'OneShot
  Destroy :: ZOPermanent -> Effect 'OneShot
  DrawCards :: ZOPlayer -> Int -> Effect 'OneShot
  EffectCase :: Case (Effect ef) -> Effect ef
  EffectContinuous :: Effect 'Continuous -> Effect 'OneShot -- 611.2
  EndTheTurn :: Effect 'OneShot
  Exile :: (IsZO zone ot, CoCard ot) => ZO zone ot -> Effect 'OneShot -- TODO: prohibit (zone == 'ZExile)
  GainAbility :: (CoAny ot, IsOTN ot) => ZO 'ZBattlefield ot -> WithThisAbility 'ZBattlefield ot -> Effect 'Continuous
  -- XXX: This is Continuous to support things like "gain control until end of turn"
  GainControl :: (CoAny ot, IsOTN ot) => ZOPlayer -> ZO 'ZBattlefield ot -> Effect 'Continuous -- TODO: restrict zone to 'ZBattlefield and 'ZStack
  GainLife :: ZOPlayer -> Int -> Effect 'OneShot -- TODO: PositiveInt
  LoseAbility :: (CoAny ot, IsOTN ot) => ZO 'ZBattlefield ot -> WithThisAbility 'ZBattlefield ot -> Effect 'Continuous
  LoseLife :: ZOPlayer -> Int -> Effect 'OneShot -- TODO: PositiveInt
  PutOntoBattlefield :: (CoPermanent ot, IsZO zone ot) => ZOPlayer -> ZO zone ot -> Effect 'OneShot -- TODO: zone /= 'ZBattlefield
  Sacrifice :: (CoPermanent ot, IsOTN ot) => ZOPlayer -> [Requirement 'ZBattlefield ot] -> Effect 'OneShot
  SearchLibrary :: (CoCard ot, IsOTN ot) => ZOPlayer {-searcher-} -> ZOPlayer {-searchee-} -> WithLinkedObject 'ZLibrary (Elect 'ResolveStage (Effect 'OneShot)) ot -> Effect 'OneShot
  Sequence :: [Effect ef] -> Effect ef
  ShuffleLibrary :: ZOPlayer -> Effect 'OneShot
  StatDelta :: ZOCreature -> Power -> Toughness -> Effect 'Continuous
  Tap :: CoPermanent ot => ZO 'ZBattlefield ot -> Effect 'OneShot
  Untap :: CoPermanent ot => ZO 'ZBattlefield ot -> Effect 'OneShot
  Until :: Elect 'ResolveStage Event OTNPlayer -> Effect 'Continuous -> Effect 'Continuous
  WithList :: IsZO zone ot => WithList (Effect ef) zone ot -> Effect ef
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
    DrawCards{} -> 9
    EffectCase{} -> 10
    EffectContinuous{} -> 11
    EndTheTurn{} -> 12
    Exile{} -> 13
    GainAbility{} -> 14
    GainControl{} -> 15
    GainLife{} -> 16
    LoseAbility{} -> 17
    LoseLife{} -> 18
    PutOntoBattlefield{} -> 19
    Sacrifice{} -> 20
    SearchLibrary{} -> 21
    Sequence{} -> 22
    ShuffleLibrary{} -> 23
    StatDelta{} -> 24
    Tap{} -> 25
    Untap{} -> 26
    Until{} -> 27
    WithList{} -> 28

--------------------------------------------------------------------------------

data Elect (s :: ElectStage) (el :: Type) (ot :: Type) :: Type where
  ActivePlayer :: (ZOPlayer -> Elect s el ot) -> Elect s el ot
  -- TODO: Add `IsZO zone ot` witness and change `'ZBattlefield` to `zone`.
  All :: IsOTN ot => WithMaskedObjects 'ZBattlefield (Elect s el) ot -> Elect s el ot
  -- TODO: Disallow `Choose` for some types of `el` using a witness arg, in particular Event and EventListener
  Choose ::
    (CoNonIntrinsicStage s, Typeable el, IsZO zone ot) =>
    ZOPlayer ->
    WithMaskedObject zone (Elect s el) ot ->
    Elect s el ot
  ChooseOption ::
    (IsUser u, IsNat n, CoNonIntrinsicStage s) =>
    ZOPlayer ->
    NatList u n Condition ->
    (Variable (Fin u n) -> Elect s el ot) ->
    Elect s el ot
  Condition :: Condition -> Elect s Condition ot
  ControllerOf :: IsZO zone OTNAny => ZO zone OTNAny -> (ZOPlayer -> Elect s el ot) -> Elect s el ot
  Cost :: Cost ot -> Elect 'IntrinsicStage (Cost ot) ot -- XXX: can this constructor be removed?
  Effect :: Typeable ef => [Effect ef] -> Elect 'ResolveStage (Effect ef) ot
  ElectActivated :: IsZO zone ot => ActivatedAbility zone ot -> Elect 'TargetStage (ActivatedAbility zone ot) ot
  ElectCardFacet :: CardFacet ot -> Elect 'IntrinsicStage (CardFacet ot) ot
  ElectCardFacet' :: CardFacet' ot -> Elect 'TargetStage (CardFacet' ot) ot
  ElectCase :: Case (Elect s el ot) -> Elect s el ot
  EndTargets :: Typeable el => Elect 'ResolveStage el ot -> Elect 'TargetStage (Elect 'ResolveStage el ot) ot
  Event :: Event -> Elect 'ResolveStage Event ot
  If :: Condition -> Elect 'ResolveStage el ot -> Else el ot -> Elect 'ResolveStage el ot -- NOTE: It is probably correct to allow this constructor with CardTypeDef usage in order to encode split cards and such.
  Listen :: EventListener -> Elect 'ResolveStage EventListener ot
  OwnerOf :: IsZO zone OTNAny => ZO zone OTNAny -> (ZOPlayer -> Elect s el ot) -> Elect s el ot
  -- TODO: Add `IsZO zone ot` witness and change `'ZBattlefield` to `zone`.
  -- TODO: Prolly allow both 'Pre and 'Post
  -- XXX: `Random` is potentially problematic when done within an aggregate Elect such as `All`...
  --    Solutions:
  --      (1) Use 'Pre instead of 'Post, but I think 'Post effects will need this
  --      (2) Introduce 'Mid for between 'Pre and 'Post to enforce it happens before aggregates
  --      (3) Say that this is OK and say that Random must come before All if want it unified. Seems good actually...
  Random ::
    IsOTN ot =>
    WithMaskedObject 'ZBattlefield (Elect 'ResolveStage el) ot ->
    Elect 'ResolveStage el ot -- Interpreted as "Arbitrary" in some contexts, such as Event and EventListener

  -- TODO: Disallow `Target` for some types of `el` using a witness arg, in particular Event and EventListener
  Target ::
    (Typeable el, IsZO zone ot) =>
    ZOPlayer ->
    WithMaskedObject zone (Elect 'TargetStage el) ot ->
    Elect 'TargetStage el ot
  VariableFromPower :: ZOCreature -> (Variable Int -> Elect 'ResolveStage el ot) -> Elect 'ResolveStage el ot
  -- TODO: It is actually possible to allow this to be IntrinsicStage by defaulting X to 0... but then `performElections`
  -- for IntrinsicStage would need to somehow be RO and RW (RO for defaulted X and RW for player choice for X). Probably
  -- possible by augmenting ElectStageRW with another type parameter. Worth it or necessary?
  VariableInt :: (Variable Int -> Elect 'TargetStage el ot) -> Elect 'TargetStage el ot
  -- TODO: Prolly can be any electStage instead of just IntrinsicStage.
  Your :: (ZOPlayer -> Elect 'IntrinsicStage el ot) -> Elect 'IntrinsicStage el ot
  deriving (Typeable)

instance ConsIndex (Elect s el ot) where
  consIndex = \case
    ActivePlayer{} -> 1
    All{} -> 2
    Choose{} -> 3
    ChooseOption{} -> 4
    Condition{} -> 5
    ControllerOf{} -> 6
    Cost{} -> 7
    Effect{} -> 8
    ElectActivated{} -> 9
    ElectCardFacet{} -> 10
    ElectCardFacet'{} -> 11
    ElectCase{} -> 12
    EndTargets{} -> 13
    Event{} -> 14
    If{} -> 15
    Listen{} -> 16
    OwnerOf{} -> 17
    Random{} -> 18
    Target{} -> 19
    VariableFromPower{} -> 20
    VariableInt{} -> 21
    Your{} -> 22

type ElectTargetedEffect ef ot = Elect 'TargetStage (Elect 'ResolveStage ef ot) ot

-- | Used to make some higher-kinded `ot` stuff work.
data ElectOT (s :: ElectStage) (liftOT :: Type -> Type) (ot :: Type) where
  ElectOT :: {unElectOT :: Elect s (liftOT ot) ot} -> ElectOT s liftOT ot
  deriving (Typeable)

--------------------------------------------------------------------------------

data Else (el :: Type) (ot :: Type) :: Type where
  ElseCost :: (el ~ Cost ot) => Elect 'ResolveStage el ot -> Else el ot
  ElseEffect :: (el ~ Effect 'OneShot) => Elect 'ResolveStage el ot -> Else el ot
  -- NOTE: Events need linear history to make sense of election costs tied to it, hence this hole.
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

--------------------------------------------------------------------------------

data Enchant (zone :: Zone) (ot :: Type) :: Type where
  Enchant :: IsZO zone ot => WithLinkedObject zone (Elect 'ResolveStage (Effect 'Continuous)) ot -> Enchant zone ot
  deriving (Typeable)

instance ConsIndex (Enchant zone ot) where
  consIndex = \case
    Enchant{} -> 1

--------------------------------------------------------------------------------

data EnchantmentType (ot :: Type) :: Type where
  Aura :: (ot ~ OTNEnchantment, IsZO zone ot') => Enchant zone ot' -> EnchantmentType ot
  deriving (Typeable)

instance ConsIndex (EnchantmentType ot) where
  consIndex = \case
    Aura{} -> 1

--------------------------------------------------------------------------------

-- This is distinct from triggered ETB effects.
data EntersStatic (zone :: Zone) (ot :: Type) :: Type where
  EntersTapped :: (CoPermanent ot, IsOTN ot) => EntersStatic 'ZBattlefield ot
  -- EntersWithCounters :: (CoPermanent ot, IsOTN ot) => CounterType ot -> Int -> EntersStatic 'ZBattlefield ot
  deriving (Typeable)

instance ConsIndex (EntersStatic zone ot) where
  consIndex = \case
    EntersTapped{} -> 1

--------------------------------------------------------------------------------

-- TODO: move out of this module
class IsZone zone => CoNonBattlefield (zone :: Zone)

-- See `Until` constructor for a place where this is used. e.g. [Pradesh Gypsies]
type Event = EventListener' Proxy

-- XXX: This should use 'Pre/PrePost instead of 'Post? e.g. [Flametongue Kavu]
type EventListener = EventListener' (Elect 'ResolveStage (Effect 'OneShot))

data EventListener' (liftOT :: Type -> Type) :: Type where
  BecomesTapped :: (CoPermanent ot, IsOTN ot, Typeable liftOT) => WithLinkedObject 'ZBattlefield liftOT ot -> EventListener' liftOT
  EntersBattlefield :: (CoPermanent ot, IsOTN ot, Typeable liftOT) => WithLinkedObject 'ZBattlefield liftOT ot -> EventListener' liftOT
  EntersNonBattlefield :: (CoNonBattlefield zone, CoCard ot, Typeable liftOT) => WithLinkedObject zone liftOT ot -> EventListener' liftOT
  Events :: [EventListener' liftOT] -> EventListener' liftOT
  SpellIsCast :: (CoSpell ot, IsOTN ot) => WithLinkedObject 'ZBattlefield liftOT ot -> EventListener' liftOT
  TimePoint :: Typeable p => TimePoint p -> liftOT OTNPlayer -> EventListener' liftOT
  deriving (Typeable)

instance ConsIndex (EventListener' liftOT) where
  consIndex = \case
    BecomesTapped{} -> 1
    EntersBattlefield{} -> 2
    EntersNonBattlefield{} -> 3
    Events{} -> 4
    SpellIsCast{} -> 5
    TimePoint{} -> 6

--------------------------------------------------------------------------------

newtype List a = List [a]
  deriving (Functor, Typeable)

instance Applicative List where
  pure = List . pure
  List f <*> List x = List $ f <*> x

--------------------------------------------------------------------------------

data NonProxy (liftOT :: Type -> Type) :: Type where
  NonProxyElectEffect :: NonProxy (Elect s (Effect ef))
  --NonProxyElectPrePostEffect :: NonProxy (Elect 'Pre (Elect 'Post (Effect 'Continuous) ot))
  deriving (Typeable)

instance ConsIndex (NonProxy liftOT) where
  consIndex = \case
    NonProxyElectEffect -> 1

--------------------------------------------------------------------------------

data Requirement (zone :: Zone) (ot :: Type) :: Type where
  ControlledBy :: IsOTN ot => ZOPlayer -> Requirement 'ZBattlefield ot
  ControlsA :: IsOTN ot => Requirement 'ZBattlefield ot -> Requirement zone OTNPlayer
  HasAbility :: IsZO zone ot => SomeZone WithThisAbility ot -> Requirement zone ot -- Non-unique differing representations will not be considered the same
  HasLandType :: IsZO zone OTNLand => LandType -> Requirement zone OTNLand
  Is :: (CoAny ot, IsZO zone ot) => ZO zone ot -> Requirement zone ot
  IsOpponentOf :: ZOPlayer -> Requirement zone OTNPlayer
  IsTapped :: (CoPermanent ot, IsOTN ot) => Requirement 'ZBattlefield ot
  Not :: IsZO zone ot => Requirement zone ot -> Requirement zone ot
  OfColors :: IsZO zone ot => Colors -> Requirement zone ot -- needs `WCard a` witness
  OwnedBy :: IsZO zone ot => ZOPlayer -> Requirement zone ot
  PlayerPays :: IsZO zone OTNPlayer => Cost OTNPlayer -> Requirement zone OTNPlayer
  RAnd :: IsZO zone ot => [Requirement zone ot] -> Requirement zone ot
  ROr :: IsZO zone ot => [Requirement zone ot] -> Requirement zone ot
  -- TODO: Try to add some combinators that go from: forall a b. [forall liftOT. Requirement x] -> Requirement (ON2 a, b)
  Req2 ::
    ( IsZO zone (OT2 a b)
    , Inst2 IsObjectType a b
    ) =>
    [Requirement zone (OT1 a)] ->
    [Requirement zone (OT1 b)] ->
    Requirement zone (OT2 a b)
  Req3 ::
    ( IsZO zone (OT3 a b c)
    , Inst3 IsObjectType a b c
    ) =>
    [Requirement zone (OT1 a)] ->
    [Requirement zone (OT1 b)] ->
    [Requirement zone (OT1 c)] ->
    Requirement zone (OT3 a b c)
  Req4 ::
    ( IsZO zone (OT4 a b c d)
    , Inst4 IsObjectType a b c d
    ) =>
    [Requirement zone (OT1 a)] ->
    [Requirement zone (OT1 b)] ->
    [Requirement zone (OT1 c)] ->
    [Requirement zone (OT1 d)] ->
    Requirement zone (OT4 a b c d)
  Req5 ::
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
    ControlledBy{} -> 1
    ControlsA{} -> 2
    HasAbility{} -> 3
    HasLandType{} -> 4
    Is{} -> 5
    IsOpponentOf{} -> 6
    IsTapped{} -> 7
    Not{} -> 8
    OfColors{} -> 9
    OwnedBy{} -> 10
    PlayerPays{} -> 11
    RAnd{} -> 12
    ROr{} -> 13
    Req2{} -> 14
    Req3{} -> 15
    Req4{} -> 16
    Req5{} -> 17

--------------------------------------------------------------------------------

-- XXX: Better to just make this take a user type `a` which can encode CardSet and Rarirty amongst other things.
data SetCard (ot :: Type) :: Type where
  SetCard :: CardSet -> Rarity -> Card ot -> SetCard ot
  deriving (Typeable)

instance ConsIndex (SetCard ot) where
  consIndex = \case
    SetCard{} -> 1

--------------------------------------------------------------------------------

-- XXX: Better to just make this take a user type `a` which can encode CardSet and Rarirty amongst other things.
data SetToken (ot :: Type) :: Type where
  SetToken :: CardSet -> Rarity -> Token ot -> SetToken ot
  deriving (Typeable)

instance ConsIndex (SetToken ot) where
  consIndex = \case
    SetToken{} -> 1

--------------------------------------------------------------------------------

-- TODO: Move all these Some* stuff to another file

data SomeOT (liftOT :: Type -> Type) (ot :: Type) :: Type where
  Some2a :: Inst2 IsObjectType a b => SomeTerm liftOT (OT1 a) -> SomeOT liftOT (OT2 a b)
  Some2b :: Inst2 IsObjectType a b => SomeTerm liftOT (OT1 b) -> SomeOT liftOT (OT2 a b)
  --
  Some5a :: Inst5 IsObjectType a b c d e => SomeTerm liftOT (OT1 a) -> SomeOT liftOT (OT5 a b c d e)
  Some5b :: Inst5 IsObjectType a b c d e => SomeTerm liftOT (OT1 b) -> SomeOT liftOT (OT5 a b c d e)
  Some5c :: Inst5 IsObjectType a b c d e => SomeTerm liftOT (OT1 c) -> SomeOT liftOT (OT5 a b c d e)
  Some5d :: Inst5 IsObjectType a b c d e => SomeTerm liftOT (OT1 d) -> SomeOT liftOT (OT5 a b c d e)
  Some5e :: Inst5 IsObjectType a b c d e => SomeTerm liftOT (OT1 e) -> SomeOT liftOT (OT5 a b c d e)
  Some5ab :: Inst5 IsObjectType a b c d e => SomeTerm liftOT (OT2 a b) -> SomeOT liftOT (OT5 a b c d e)
  Some5ad :: Inst5 IsObjectType a b c d e => SomeTerm liftOT (OT2 a d) -> SomeOT liftOT (OT5 a b c d e)
  Some5bc :: Inst5 IsObjectType a b c d e => SomeTerm liftOT (OT2 b c) -> SomeOT liftOT (OT5 a b c d e)
  --
  Some6a :: Inst6 IsObjectType a b c d e f => SomeTerm liftOT (OT1 a) -> SomeOT liftOT (OT6 a b c d e f)
  Some6b :: Inst6 IsObjectType a b c d e f => SomeTerm liftOT (OT1 b) -> SomeOT liftOT (OT6 a b c d e f)
  Some6c :: Inst6 IsObjectType a b c d e f => SomeTerm liftOT (OT1 c) -> SomeOT liftOT (OT6 a b c d e f)
  Some6d :: Inst6 IsObjectType a b c d e f => SomeTerm liftOT (OT1 d) -> SomeOT liftOT (OT6 a b c d e f)
  Some6e :: Inst6 IsObjectType a b c d e f => SomeTerm liftOT (OT1 e) -> SomeOT liftOT (OT6 a b c d e f)
  Some6f :: Inst6 IsObjectType a b c d e f => SomeTerm liftOT (OT1 f) -> SomeOT liftOT (OT6 a b c d e f)
  Some6ab :: Inst6 IsObjectType a b c d e f => SomeTerm liftOT (OT2 a b) -> SomeOT liftOT (OT6 a b c d e f)
  Some6bc :: Inst6 IsObjectType a b c d e f => SomeTerm liftOT (OT2 b c) -> SomeOT liftOT (OT6 a b c d e f)
  -- TODO: Write a script to generate other Some6xy flavors
  deriving (Typeable)

data SomeTerm (liftOT :: Type -> Type) (ot :: Type) :: Type where
  SomeArtifact :: liftOT OTNArtifact -> SomeTerm liftOT OTNArtifact
  SomeCreature :: liftOT OTNCreature -> SomeTerm liftOT OTNCreature
  SomeEnchantment :: liftOT OTNEnchantment -> SomeTerm liftOT OTNEnchantment
  SomeInstant :: liftOT OTNInstant -> SomeTerm liftOT OTNInstant
  SomeLand :: liftOT OTNLand -> SomeTerm liftOT OTNLand
  SomePlaneswalker :: liftOT OTNPlaneswalker -> SomeTerm liftOT OTNPlaneswalker
  SomeSorcery :: liftOT OTNSorcery -> SomeTerm liftOT OTNSorcery
  SomeArtifactCreature :: liftOT OTNArtifactCreature -> SomeTerm liftOT OTNArtifactCreature
  SomeArtifactLand :: liftOT OTNArtifactLand -> SomeTerm liftOT OTNArtifactLand
  SomeEnchantmentCreature :: liftOT OTNEnchantmentCreature -> SomeTerm liftOT OTNEnchantmentCreature
  deriving (Typeable)

data SomeCard (ot :: Type) :: Type where
  SomeCard :: (ot ~ OTN x, IsSpecificCard ot) => SomeCard ot -> SomeCard ot
  SomeDoubleSidedCard1 :: (ot ~ OTN x, ot' ~ OTN y, Inst2 IsSpecificCard ot ot') => SomeCard ot -> SomeCard ot' -> SomeCard ot
  SomeDoubleSidedCard2 :: (ot ~ OTN x, ot' ~ OTN y, Inst2 IsSpecificCard ot ot') => SomeCard ot' -> SomeCard ot -> SomeCard ot
  SomeSplitCard1 :: (ot ~ OTN x, ot' ~ OTN y, Inst2 IsSpecificCard ot ot') => SomeCard ot -> SomeCard ot' -> SomeCard ot
  SomeSplitCard2 :: (ot ~ OTN x, ot' ~ OTN y, Inst2 IsSpecificCard ot ot') => SomeCard ot' -> SomeCard ot -> SomeCard ot

type SomeToken = SomeOT Token

type SomeCardOrToken ot = Either (SomeCard ot) (SomeToken ot)

mapSomeOT ::
  forall liftOT liftOT' ot.
  SomeOT liftOT ot ->
  (forall ot'. liftOT ot' -> Maybe (liftOT' ot')) ->
  Maybe (SomeOT liftOT' ot)
mapSomeOT some f = case some of
  Some2a term -> Some2a <$> goTerm term
  Some2b term -> Some2b <$> goTerm term
  Some5a term -> Some5a <$> goTerm term
  Some5b term -> Some5b <$> goTerm term
  Some5c term -> Some5c <$> goTerm term
  Some5d term -> Some5d <$> goTerm term
  Some5e term -> Some5e <$> goTerm term
  Some5ab term -> Some5ab <$> goTerm term
  Some5ad term -> Some5ad <$> goTerm term
  Some5bc term -> Some5bc <$> goTerm term
  Some6a term -> Some6a <$> goTerm term
  Some6b term -> Some6b <$> goTerm term
  Some6c term -> Some6c <$> goTerm term
  Some6d term -> Some6d <$> goTerm term
  Some6e term -> Some6e <$> goTerm term
  Some6f term -> Some6f <$> goTerm term
  Some6ab term -> Some6ab <$> goTerm term
  Some6bc term -> Some6bc <$> goTerm term
 where
  goTerm :: SomeTerm liftOT ot' -> Maybe (SomeTerm liftOT' ot')
  goTerm = \case
    SomeArtifact x -> SomeArtifact <$> f x
    SomeCreature x -> SomeCreature <$> f x
    SomeEnchantment x -> SomeEnchantment <$> f x
    SomeInstant x -> SomeInstant <$> f x
    SomeLand x -> SomeLand <$> f x
    SomePlaneswalker x -> SomePlaneswalker <$> f x
    SomeSorcery x -> SomeSorcery <$> f x
    SomeArtifactCreature x -> SomeArtifactCreature <$> f x
    SomeArtifactLand x -> SomeArtifactLand <$> f x
    SomeEnchantmentCreature x -> SomeEnchantmentCreature <$> f x

fromSomeOT ::
  forall liftOT ot x.
  SomeOT liftOT ot ->
  (forall ot'. liftOT ot' -> x) ->
  x
fromSomeOT some f = case some of
  Some2a term -> goTerm term
  Some2b term -> goTerm term
  Some5a term -> goTerm term
  Some5b term -> goTerm term
  Some5c term -> goTerm term
  Some5d term -> goTerm term
  Some5e term -> goTerm term
  Some5ab term -> goTerm term
  Some5ad term -> goTerm term
  Some5bc term -> goTerm term
  Some6a term -> goTerm term
  Some6b term -> goTerm term
  Some6c term -> goTerm term
  Some6d term -> goTerm term
  Some6e term -> goTerm term
  Some6f term -> goTerm term
  Some6ab term -> goTerm term
  Some6bc term -> goTerm term
 where
  goTerm :: SomeTerm liftOT ot' -> x
  goTerm = \case
    SomeArtifact x -> f x
    SomeCreature x -> f x
    SomeEnchantment x -> f x
    SomeInstant x -> f x
    SomeLand x -> f x
    SomePlaneswalker x -> f x
    SomeSorcery x -> f x
    SomeArtifactCreature x -> f x
    SomeArtifactLand x -> f x
    SomeEnchantmentCreature x -> f x

--------------------------------------------------------------------------------

data SomeZone (liftZOT :: Zone -> Type -> Type) (ot :: Type) :: Type where
  SomeZone :: (ot ~ OTN x, IsZO zone ot) => liftZOT zone ot -> SomeZone liftZOT ot
  SomeZone2 :: (IsZO zone ot1, IsZO zone ot2) => liftZOT zone (ot1, ot2) -> SomeZone liftZOT (ot1, ot2)
  deriving (Typeable)

instance ConsIndex (SomeZone liftZOT ot) where
  consIndex = \case
    SomeZone{} -> 1
    SomeZone2{} -> 2

--------------------------------------------------------------------------------

data StaticAbility (zone :: Zone) (ot :: Type) :: Type where
  As :: (ot ~ OTN x, IsOTN ot) => Elect 'ResolveStage EventListener ot -> StaticAbility 'ZBattlefield ot -- 603.6d: not a triggered ability
  -- XXX: BestowPre and BestowPost
  Bestow :: ot ~ OTNEnchantmentCreature => Elect 'IntrinsicStage (Cost ot) ot -> Enchant 'ZBattlefield OTNCreature -> StaticAbility 'ZBattlefield ot
  CantBlock :: ot ~ OTNCreature => StaticAbility 'ZBattlefield ot
  Defender :: ot ~ OTNCreature => StaticAbility 'ZBattlefield ot
  Enters :: IsZO zone ot => EntersStatic zone ot -> StaticAbility zone ot
  FirstStrike :: ot ~ OTNCreature => StaticAbility 'ZBattlefield ot
  Flying :: ot ~ OTNCreature => StaticAbility 'ZBattlefield ot
  Fuse :: IsOTN ot => StaticAbility 'ZHand (ot, ot) -- TODO: Add witness or constraint for OTNInstant or OTNSorcery
  Haste :: ot ~ OTNCreature => StaticAbility 'ZBattlefield ot
  Landwalk :: ot ~ OTNCreature => [Requirement 'ZBattlefield OTNLand] -> StaticAbility 'ZBattlefield ot
  Phasing :: CoPermanent ot => StaticAbility 'ZBattlefield ot
  StaticContinuous :: (ot ~ OTN x, IsOTN ot) => Elect 'ResolveStage (Effect 'Continuous) ot -> StaticAbility 'ZBattlefield ot -- 611.3
  -- XXX: SuspendPre and SuspendPost
  Suspend :: (ot ~ OTN x, IsOTN ot) => Int -> Elect 'IntrinsicStage (Cost ot) ot -> StaticAbility 'ZBattlefield ot -- PositiveInt
  Trample :: ot ~ OTNCreature => StaticAbility 'ZBattlefield ot
  deriving (Typeable)

instance ConsIndex (StaticAbility zone ot) where
  consIndex = \case
    As{} -> 1
    Bestow{} -> 2
    CantBlock{} -> 3
    Defender{} -> 4
    Enters{} -> 5
    FirstStrike{} -> 6
    Flying{} -> 7
    Fuse{} -> 8
    Haste{} -> 9
    Landwalk{} -> 10
    Phasing{} -> 11
    StaticContinuous{} -> 12
    Suspend{} -> 13
    Trample{} -> 14

--------------------------------------------------------------------------------

data Token (ot :: Type) :: Type where
  Token :: (ot ~ OTN x, CoPermanent ot, IsSpecificCard ot) => Card ot -> Token ot
  deriving (Typeable)

instance ConsIndex (Token ot) where
  consIndex = \case
    Token{} -> 1

instance HasCardName (Token ot) where
  getCardName = \case
    Token card -> getCardName card

--------------------------------------------------------------------------------

-- https://www.mtgsalvation.com/forums/magic-fundamentals/magic-rulings/magic-rulings-archives/611601-whenever-what-does-it-mean?comment=3
-- https://www.reddit.com/r/magicTCG/comments/asmecb/noob_question_difference_between_as_and_when/
data TriggeredAbility (zone :: Zone) (ot :: Type) :: Type where
  When :: IsZO 'ZBattlefield ot => Elect 'ResolveStage EventListener ot -> TriggeredAbility 'ZBattlefield ot
  deriving (Typeable)

instance ConsIndex (TriggeredAbility zone ot) where
  consIndex = \case
    When{} -> 1

--------------------------------------------------------------------------------

-- "Linked" is used to denote that the object fed into the continuation has the same `ot`
-- as WithLinkedObject's `ot` type.
data WithLinkedObject (zone :: Zone) (liftOT :: Type -> Type) (ot :: Type) :: Type where
  Linked1 ::
    (ot ~ OT1 a, IsOTN ot, Inst1 IsObjectType a) =>
    NonProxy liftOT ->
    [Requirement zone ot] ->
    (ZO zone ot -> liftOT ot) ->
    WithLinkedObject zone liftOT ot
  Linked2 ::
    (ot ~ OT2 a b, IsOTN ot, Inst2 IsObjectType a b) =>
    NonProxy liftOT ->
    [Requirement zone ot] ->
    (ZO zone ot -> liftOT ot) ->
    WithLinkedObject zone liftOT ot
  Linked3 ::
    (ot ~ OT3 a b c, IsOTN ot, Inst3 IsObjectType a b c) =>
    NonProxy liftOT ->
    [Requirement zone ot] ->
    (ZO zone ot -> liftOT ot) ->
    WithLinkedObject zone liftOT ot
  Linked4 ::
    (ot ~ OT4 a b c d, IsOTN ot, Inst4 IsObjectType a b c d) =>
    NonProxy liftOT ->
    [Requirement zone ot] ->
    (ZO zone ot -> liftOT ot) ->
    WithLinkedObject zone liftOT ot
  Linked5 ::
    (ot ~ OT5 a b c d e, IsOTN ot, Inst5 IsObjectType a b c d e) =>
    NonProxy liftOT ->
    [Requirement zone ot] ->
    (ZO zone ot -> liftOT ot) ->
    WithLinkedObject zone liftOT ot
  deriving (Typeable)

instance ConsIndex (WithLinkedObject zone liftOT ot) where
  consIndex = \case
    Linked1{} -> 1
    Linked2{} -> 2
    Linked3{} -> 3
    Linked4{} -> 4
    Linked5{} -> 5

--------------------------------------------------------------------------------

data WithList (ret :: Type) (zone :: Zone) (ot :: Type) where
  CountOf :: (IsZO zone ot, Typeable ret) => List (ZO zone ot) -> (Variable Int -> ret) -> WithList ret zone ot
  Each :: (IsZO zone ot, Typeable ret) => List (ZO zone ot) -> (ZO zone ot -> ret) -> WithList ret zone ot
  SuchThat :: (IsZO zone ot, Typeable ret) => [Requirement zone ot] -> WithList ret zone ot -> WithList ret zone ot

instance ConsIndex (WithList ret zone ot) where
  consIndex = \case
    CountOf{} -> 1
    Each{} -> 2
    SuchThat{} -> 3

--------------------------------------------------------------------------------

-- "Masked" is used to denote that the object fed into the continuation has an `ot'` that
-- is independent from WithMaskedObject's `ot` type (aka the `ot` in the continuation result).
data WithMaskedObject (zone :: Zone) (liftOT :: Type -> Type) (ot :: Type) :: Type where
  Masked1 ::
    (Typeable (liftOT ot), ot' ~ OT1 a, IsOTN ot', Inst1 IsObjectType a) =>
    [Requirement zone ot'] ->
    (ZO zone ot' -> liftOT ot) ->
    WithMaskedObject zone liftOT ot
  Masked2 ::
    (Typeable (liftOT ot), ot' ~ OT2 a b, IsOTN ot', Inst2 IsObjectType a b) =>
    [Requirement zone ot'] ->
    (ZO zone ot' -> liftOT ot) ->
    WithMaskedObject zone liftOT ot
  Masked3 ::
    (Typeable (liftOT ot), ot' ~ OT3 a b c, IsOTN ot', Inst3 IsObjectType a b c) =>
    [Requirement zone ot'] ->
    (ZO zone ot' -> liftOT ot) ->
    WithMaskedObject zone liftOT ot
  Masked4 ::
    (Typeable (liftOT ot), ot' ~ OT4 a b c d, IsOTN ot', Inst4 IsObjectType a b c d) =>
    [Requirement zone ot'] ->
    (ZO zone ot' -> liftOT ot) ->
    WithMaskedObject zone liftOT ot
  Masked5 ::
    (Typeable (liftOT ot), ot' ~ OT5 a b c d e, IsOTN ot', Inst5 IsObjectType a b c d e) =>
    [Requirement zone ot'] ->
    (ZO zone ot' -> liftOT ot) ->
    WithMaskedObject zone liftOT ot
  Masked6 ::
    (Typeable (liftOT ot), ot' ~ OT6 a b c d e f, IsOTN ot', Inst6 IsObjectType a b c d e f) =>
    [Requirement zone ot'] ->
    (ZO zone ot' -> liftOT ot) ->
    WithMaskedObject zone liftOT ot
  deriving (Typeable)

instance ConsIndex (WithMaskedObject zone liftOT ot) where
  consIndex = \case
    Masked1{} -> 1
    Masked2{} -> 2
    Masked3{} -> 3
    Masked4{} -> 4
    Masked5{} -> 5
    Masked6{} -> 6

--------------------------------------------------------------------------------

data WithMaskedObjects (zone :: Zone) (liftOT :: Type -> Type) (ot :: Type) :: Type where
  Maskeds1 ::
    (Typeable (liftOT ot), ot' ~ OT1 a, IsOTN ot', Inst1 IsObjectType a) =>
    [Requirement zone ot'] ->
    (List (ZO zone ot') -> liftOT ot) ->
    WithMaskedObjects zone liftOT ot
  Maskeds2 ::
    (Typeable (liftOT ot), ot' ~ OT2 a b, IsOTN ot', Inst2 IsObjectType a b) =>
    [Requirement zone ot'] ->
    (List (ZO zone ot') -> liftOT ot) ->
    WithMaskedObjects zone liftOT ot
  Maskeds3 ::
    (Typeable (liftOT ot), ot' ~ OT3 a b c, IsOTN ot', Inst3 IsObjectType a b c) =>
    [Requirement zone ot'] ->
    (List (ZO zone ot') -> liftOT ot) ->
    WithMaskedObjects zone liftOT ot
  Maskeds4 ::
    (Typeable (liftOT ot), ot' ~ OT4 a b c d, IsOTN ot', Inst4 IsObjectType a b c d) =>
    [Requirement zone ot'] ->
    (List (ZO zone ot') -> liftOT ot) ->
    WithMaskedObjects zone liftOT ot
  Maskeds5 ::
    (Typeable (liftOT ot), ot' ~ OT5 a b c d e, IsOTN ot', Inst5 IsObjectType a b c d e) =>
    [Requirement zone ot'] ->
    (List (ZO zone ot') -> liftOT ot) ->
    WithMaskedObjects zone liftOT ot
  Maskeds6 ::
    (Typeable (liftOT ot), ot' ~ OT6 a b c d e f, IsOTN ot', Inst6 IsObjectType a b c d e f) =>
    [Requirement zone ot'] ->
    (List (ZO zone ot') -> liftOT ot) ->
    WithMaskedObjects zone liftOT ot
  deriving (Typeable)

instance ConsIndex (WithMaskedObjects zone liftOT ot) where
  consIndex = \case
    Maskeds1{} -> 1
    Maskeds2{} -> 2
    Maskeds3{} -> 3
    Maskeds4{} -> 4
    Maskeds5{} -> 5
    Maskeds6{} -> 6

--------------------------------------------------------------------------------

-- NOTE: At the moment there don't exist any cards with more than 3 facets. That said, extending
-- to This4 and This5, we get OTNPermanent support, which is useful. It lets the engine do some
-- things by only specifying OTNPermanent and not all the combinations of OT's that constitute
-- OTNPermanent. Currently this is leveraged to by the UI through `getIntrinsicManaAbilities` to
-- discover the mana abilities of permanents without caring too much about the specific type.
--
-- NOTE: I don't think it makes sense for this to ever support split cards directly through
-- something like `WithThis zone (ot1, ot2)` pairings since split cards (et al) always resolve
-- into a single object... unless fused, but note that fused cards have ot1 == ot2. So perhaps
-- I only support the case for `WithThis zone (ot, ot)`. Normally to "support" split cards with
-- differing ot1 and ot2, each split card constituent would boil down into a normal non-split
-- card, in which case we can just use `WithThis zone ot1` or `WithThis zone ot2` respectively.
data WithThis (liftOT :: Type -> Type) (zone :: Zone) (ot :: Type) :: Type where
  -- SplitThis2 ::
  --   (IsOTN ot1, IsOTN ot2) =>
  --   (ZO zone (ot1, ot2) -> liftOT (ot1, ot2)) ->
  --   WithThis zone liftOT (ot1, ot2)
  This1 ::
    (IsOTN (OT1 a), Inst1 IsObjectType a) =>
    (ZO zone (OT1 a) -> liftOT (OT1 a)) ->
    WithThis liftOT zone (OT1 a)
  This2 ::
    (IsOTN (OT2 a b), Inst2 IsObjectType a b) =>
    -- TODO: Add a additional full (ZO zone (OT2 a b)) to the input tuple?
    -- TODO: Introduce a `This ot` record type to access its constituents.
    --       Prolly can also add ToObjectN instances (cool!).
    ((ZO zone (OT1 a), ZO zone (OT1 b)) -> liftOT (OT2 a b)) ->
    WithThis liftOT zone (OT2 a b)
  This3 ::
    (IsOTN (OT3 a b c), Inst3 IsObjectType a b c) =>
    ((ZO zone (OT1 a), ZO zone (OT1 b), ZO zone (OT1 c)) -> liftOT (OT3 a b c)) ->
    WithThis liftOT zone (OT3 a b c)
  This4 ::
    (IsOTN (OT4 a b c d), Inst4 IsObjectType a b c d) =>
    ((ZO zone (OT1 a), ZO zone (OT1 b), ZO zone (OT1 c), ZO zone (OT1 d)) -> liftOT (OT4 a b c d)) ->
    WithThis liftOT zone (OT4 a b c d)
  This5 ::
    (IsOTN (OT5 a b c d e), Inst5 IsObjectType a b c d e) =>
    ((ZO zone (OT1 a), ZO zone (OT1 b), ZO zone (OT1 c), ZO zone (OT1 d), ZO zone (OT1 e)) -> liftOT (OT5 a b c d e)) ->
    WithThis liftOT zone (OT5 a b c d e)
  deriving (Typeable)

instance ConsIndex (WithThis zone liftOT ot) where
  consIndex = \case
    This1{} -> 1
    This2{} -> 2
    This3{} -> 3
    This4{} -> 4
    This5{} -> 5

-- XXX: Can this be changed to ResolveStage if I remove WithThis and use an Elect This constructor?
type WithThisActivated zone = WithThis (ElectOT 'TargetStage (ActivatedAbility zone)) zone

type WithThisOneShot ot = WithThis (Elect 'ResolveStage (Effect 'OneShot)) 'ZStack ot

type WithThisStatic zone = WithThis (StaticAbility zone) zone

type WithThisTriggered zone = WithThis (TriggeredAbility zone) zone

-- | Used to make some higher-kinded `zone` stuff work.
data WithThisZ (liftZOT :: Zone -> Type -> Type) (zone :: Zone) (ot :: Type) where
  WithThisZ :: IsZO zone ot => WithThis (liftZOT zone) zone ot -> WithThisZ liftZOT zone ot
  deriving (Typeable)

-- This is factored with the constructors expanded out here to enable pattern matching
-- on the ability type without dealing with continuation crap.
data WithThisAbility (zone :: Zone) (ot :: Type) where
  WithThisActivated :: IsZO zone ot => WithThisActivated zone ot -> WithThisAbility zone ot
  WithThisStatic :: IsZO zone ot => WithThisStatic zone ot -> WithThisAbility zone ot
  WithThisTriggered :: IsZO zone ot => WithThisTriggered zone ot -> WithThisAbility zone ot
  deriving (Typeable)

instance ConsIndex (WithThisAbility zone ot) where
  consIndex = \case
    WithThisActivated{} -> 1
    WithThisStatic{} -> 2
    WithThisTriggered{} -> 3

--------------------------------------------------------------------------------
