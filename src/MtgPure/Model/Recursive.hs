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
  Case (..),
  Condition (..),
  Cost (..),
  Effect (..),
  Elect (..),
  ElectPrePost,
  Else (..),
  Enchant (..),
  EnchantmentType (..),
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
  Some (..),
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
  WithThisActivated,
  WithThisOneShot,
  WithThisTriggered,
  YourCardFacet (..),
  pattern CFalse,
  pattern CTrue,
  fromSome,
  mapSome,
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
import safe MtgPure.Model.LandType (LandType)
import safe MtgPure.Model.Loyalty (Loyalty)
import safe MtgPure.Model.Mana (Snow (..))
import safe MtgPure.Model.ManaCost (ManaCost)
import safe MtgPure.Model.ManaPool (ManaPool)
import MtgPure.Model.Object.IndexOT (IndexOT)
import safe MtgPure.Model.Object.IsObjectType (IsObjectType)
import safe MtgPure.Model.Object.OTN (OT1, OT2, OT3, OT4, OT5, OT6, OTN)
import safe MtgPure.Model.Object.OTNAliases (
  OTActivatedOrTriggeredAbility,
  OTAny,
  OTArtifact,
  OTArtifactCreature,
  OTArtifactLand,
  OTCreature,
  OTDamageSource,
  OTEnchantment,
  OTEnchantmentCreature,
  OTInstant,
  OTLand,
  OTPermanent,
  OTPlaneswalker,
  OTPlayer,
  OTSorcery,
  OTSpell,
 )
import safe MtgPure.Model.Object.Singleton.Any (WAny)
import safe MtgPure.Model.Object.Singleton.Card (WCard)
import safe MtgPure.Model.Object.Singleton.Permanent (CoPermanent, WPermanent)
import safe MtgPure.Model.Object.Singleton.Spell (WSpell (..))
import safe MtgPure.Model.Power (Power)
import safe MtgPure.Model.PrePost (IsPrePost, PrePost (..))
import safe MtgPure.Model.Rarity (Rarity)
import safe MtgPure.Model.TimePoint (TimePoint)
import safe MtgPure.Model.Toughness (Toughness)
import safe MtgPure.Model.Variable (Var (Var), Variable)
import safe MtgPure.Model.Zone (IsZone, Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (
  IsOT,
  IsZO,
  ZO,
  ZOCreature,
  ZOCreaturePlayerPlaneswalker,
  ZOPermanent,
  ZOPlayer,
 )

----------------------------------------

-- Semantics legend:
--
-- "ot is exactly (a,b,c,...)"
--    Ability ot (experimenting with engine not having this "exact" notion)
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
  Activated :: IsZO zone ot => WithThisActivated zone ot -> Ability ot
  Static :: (IsZone zone, IndexOT ot) => StaticAbility zone ot -> Ability ot
  Triggered :: IsZO zone ot => WithThisTriggered zone ot -> Ability ot
  deriving (Typeable)

instance ConsIndex (Ability ot) where
  consIndex = \case
    Activated{} -> 1
    Static{} -> 2
    Triggered{} -> 3

----------------------------------------

data ActivatedAbility (zone :: Zone) (ot :: Type) :: Type where
  Ability ::
    IsZO zone ot =>
    { activated_cost :: Cost ot
    , activated_effect :: Elect 'Post (Effect 'OneShot) ot
    } ->
    ActivatedAbility zone ot
  deriving (Typeable)

instance ConsIndex (ActivatedAbility zone ot) where
  consIndex = \case
    Ability{} -> 1

----------------------------------------

-- data ArtifactType (ot :: Type) :: Type where
--   Vehicle :: ArtifactType ot -- TODO: Stuff crew mechanic into here
--   deriving (Bounded, Enum, Eq, Ord, Show)

----------------------------------------

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
  ArtifactCard :: OTArtifact ~ ot => SpecificCard ot
  ArtifactCreatureCard :: OTArtifactCreature ~ ot => SpecificCard ot
  ArtifactLandCard :: OTArtifactLand ~ ot => SpecificCard ot
  CreatureCard :: OTCreature ~ ot => SpecificCard ot
  EnchantmentCard :: OTEnchantment ~ ot => SpecificCard ot
  EnchantmentCreatureCard :: OTEnchantmentCreature ~ ot => SpecificCard ot
  InstantCard :: OTInstant ~ ot => SpecificCard ot
  LandCard :: OTLand ~ ot => SpecificCard ot
  PlaneswalkerCard :: OTPlaneswalker ~ ot => SpecificCard ot
  SorceryCard :: OTSorcery ~ ot => SpecificCard ot
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

class IsOT ot => IsSpecificCard (ot :: Type) where
  singSpecificCard :: SpecificCard ot

instance IsSpecificCard OTArtifact where
  singSpecificCard = ArtifactCard

instance IsSpecificCard OTArtifactCreature where
  singSpecificCard = ArtifactCreatureCard

instance IsSpecificCard OTArtifactLand where
  singSpecificCard = ArtifactLandCard

instance IsSpecificCard OTCreature where
  singSpecificCard = CreatureCard

instance IsSpecificCard OTEnchantment where
  singSpecificCard = EnchantmentCard

instance IsSpecificCard OTEnchantmentCreature where
  singSpecificCard = EnchantmentCreatureCard

instance IsSpecificCard OTInstant where
  singSpecificCard = InstantCard

instance IsSpecificCard OTLand where
  singSpecificCard = LandCard

instance IsSpecificCard OTPlaneswalker where
  singSpecificCard = PlaneswalkerCard

instance IsSpecificCard OTSorcery where
  singSpecificCard = SorceryCard

----------------------------------------

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

----------------------------------------

data AnyToken :: Type where
  AnyToken :: IsSpecificCard ot => Token ot -> AnyToken
  deriving (Typeable)

instance ConsIndex AnyToken where
  consIndex = \case
    AnyToken{} -> 1

instance HasCardName AnyToken where
  getCardName = \case
    AnyToken token -> getCardName token

----------------------------------------

data Card (ot :: Type) :: Type where
  Card :: (ot ~ OTN x, IsSpecificCard ot) => CardName -> YourCardFacet ot -> Card ot
  DoubleSidedCard :: Inst2 IsSpecificCard ot1 ot2 => Card ot1 -> Card ot2 -> Card (ot1, ot2)
  SplitCard ::
    Inst2 IsSpecificCard ot1 ot2 =>
    { splitCard_card1 :: Card ot1
    , splitCard_card2 :: Card ot2
    , splitCard_abilities :: [Ability (ot1, ot2)]
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

----------------------------------------

-- TODO:
-- The one-shot facets need to be split into two parts:
--    * A static part
--    * A dynamic part
-- The reason for this is that the engine and clients will need to know things
-- like color, card super types, and card subtypes before doing any elections.
-- The YourCardFacet type will hold both flavors in its one-shot constructor flavors.
data CardFacet (ot :: Type) :: Type where
  ArtifactFacet ::
    { artifact_colors :: Colors
    , artifact_cost :: Cost OTArtifact
    , artifact_artifactTypes :: [ArtifactType]
    , artifact_creatureTypes :: [CreatureType]
    , artifact_abilities :: [Ability OTArtifact]
    } ->
    CardFacet OTArtifact
  ArtifactCreatureFacet ::
    { artifactCreature_colors :: Colors
    , artifactCreature_cost :: Cost OTArtifactCreature
    , artifactCreature_artifactTypes :: [ArtifactType]
    , artifactCreature_creatureTypes :: [CreatureType]
    , artifactCreature_power :: Power
    , artifactCreature_toughness :: Toughness
    , artifactCreature_artifactAbilities :: [Ability OTArtifact]
    , artifactCreature_creatureAbilities :: [Ability OTCreature]
    , artifactCreature_artifactCreatureAbilities :: [Ability OTArtifactCreature]
    } ->
    CardFacet OTArtifactCreature
  ArtifactLandFacet ::
    { artifactLand_artifactTypes :: [ArtifactType]
    , artifactLand_creatureTypes :: [CreatureType]
    , artifactLand_landTypes :: [LandType]
    , artifactLand_artifactAbilities :: [Ability OTArtifact]
    , artifactLand_landAbilities :: [Ability OTLand]
    , artifactLand_artifactLandAbilities :: [Ability OTArtifactLand]
    } ->
    CardFacet OTArtifactLand
  CreatureFacet ::
    { creature_colors :: Colors
    , creature_cost :: Cost OTCreature
    , creature_creatureTypes :: [CreatureType]
    , creature_power :: Power
    , creature_toughness :: Toughness
    , creature_abilities :: [Ability OTCreature]
    } ->
    CardFacet OTCreature
  EnchantmentFacet ::
    { enchantment_colors :: Colors
    , enchantment_cost :: Cost OTEnchantment
    , enchantment_creatureTypes :: [CreatureType]
    , enchantment_enchantmentTypes :: [EnchantmentType OTEnchantment]
    , enchantment_abilities :: [Ability OTEnchantment]
    } ->
    CardFacet OTEnchantment
  EnchantmentCreatureFacet ::
    { enchantmentCreature_colors :: Colors
    , enchantmentCreature_cost :: Cost OTEnchantmentCreature
    , enchantmentCreature_creatureTypes :: [CreatureType]
    , enchantmentCreature_enchantmentTypes :: [EnchantmentType OTEnchantmentCreature]
    , enchantmentCreature_power :: Power
    , enchantmentCreature_toughness :: Toughness
    , enchantmentCreature_creatureAbilities :: [Ability OTCreature]
    , enchantmentCreature_enchantmentAbilities :: [Ability OTEnchantment]
    , enchantmentCreature_enchantmentCreatureAbilities :: [Ability OTEnchantmentCreature]
    } ->
    CardFacet OTEnchantmentCreature
  InstantFacet ::
    { instant_colors :: Colors
    , instant_cost :: Cost OTInstant
    , instant_creatureTypes :: [CreatureType]
    , -- instant_oneShotTypes :: [OneShotType] e.g. Arcane
      instant_abilities :: [Ability OTInstant]
    , instant_effect :: WithThisOneShot OTInstant
    } ->
    CardFacet OTInstant
  LandFacet ::
    { land_creatureTypes :: [CreatureType]
    , land_landTypes :: [LandType]
    , land_abilities :: [Ability OTLand]
    } ->
    CardFacet OTLand
  PlaneswalkerFacet ::
    { planeswalker_colors :: Colors
    , planeswalker_cost :: Cost OTPlaneswalker
    , planeswalker_loyalty :: Loyalty
    , planeswalker_abilities :: [Ability OTPlaneswalker]
    } ->
    CardFacet OTPlaneswalker
  SorceryFacet ::
    { sorcery_colors :: Colors
    , sorcery_cost :: Cost OTSorcery
    , sorcery_creatureTypes :: [CreatureType]
    , -- instant_oneShotTypes :: [OneShotType] e.g. Arcane
      sorcery_abilities :: [Ability OTSorcery]
    , sorcery_effect :: WithThisOneShot OTSorcery
    } ->
    CardFacet OTSorcery
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

----------------------------------------

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

pattern CFalse :: Condition
pattern CFalse = COr []

pattern CTrue :: Condition
pattern CTrue = CAnd []

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
  CostCase :: Case (Cost ot) -> Cost ot
  DiscardRandomCost :: Int -> Cost ot -- TODO: PositiveInt
  LoyaltyCost :: Loyalty -> Cost OTPlaneswalker
  ManaCost :: ManaCost 'Var -> Cost ot
  OrCosts :: [Cost ot] -> Cost ot
  PayLife :: Int -> Cost ot -- TODO: PositiveInt
  SacrificeCost :: IsZO 'ZBattlefield ot' => WPermanent ot' -> [Requirement 'ZBattlefield ot'] -> Cost ot
  TapCost :: (CoPermanent ot', IsZO 'ZBattlefield ot') => [Requirement 'ZBattlefield ot'] -> Cost ot
  deriving (Typeable)

instance ConsIndex (Cost ot) where
  consIndex = \case
    AndCosts{} -> 1
    CostCase{} -> 2
    DiscardRandomCost{} -> 3
    LoyaltyCost{} -> 4
    ManaCost{} -> 5
    OrCosts{} -> 6
    PayLife{} -> 7
    SacrificeCost{} -> 8
    TapCost{} -> 9

----------------------------------------

data Effect (ef :: EffectType) :: Type where
  AddMana :: ZOPlayer -> ManaPool 'NonSnow -> Effect 'OneShot -- NOTE: Engine will reinterpret as Snow when source is Snow.
  AddToBattlefield :: IsOT ot => WPermanent ot -> ZOPlayer -> Token ot -> Effect 'OneShot
  CantBeRegenerated :: ZOCreature -> Effect 'Continuous
  ChangeTo :: (ot ~ OTN x, IsOT ot) => WPermanent ot -> ZOPermanent -> Card ot -> Effect 'Continuous
  CounterAbility :: ZO 'ZStack OTActivatedOrTriggeredAbility -> Effect 'OneShot
  CounterSpell :: ZO 'ZStack OTSpell -> Effect 'OneShot
  DealDamage :: IsZO zone OTDamageSource => ZO zone OTDamageSource -> ZOCreaturePlayerPlaneswalker -> Damage 'Var -> Effect 'OneShot
  Destroy :: ZOPermanent -> Effect 'OneShot
  DrawCards :: ZOPlayer -> Int -> Effect 'OneShot
  EffectCase :: Case (Effect ef) -> Effect ef
  EffectContinuous :: Effect 'Continuous -> Effect 'OneShot -- 611.2
  GainAbility :: IsOT ot => WAny ot -> ZO 'ZBattlefield ot -> Ability ot -> Effect 'Continuous
  LoseAbility :: IsOT ot => WAny ot -> ZO 'ZBattlefield ot -> Ability ot -> Effect 'Continuous
  PutOntoBattlefield :: IsZO zone ot => WPermanent ot -> ZOPlayer -> ZO zone ot -> Effect 'OneShot -- TODO: zone /= 'ZBattlefield
  Sacrifice :: IsOT ot => WPermanent ot -> ZOPlayer -> [Requirement 'ZBattlefield ot] -> Effect 'OneShot
  SearchLibrary :: IsOT ot => WCard ot -> ZOPlayer -> WithLinkedObject 'ZLibrary (Elect 'Post (Effect 'OneShot)) ot -> Effect 'OneShot
  Sequence :: [Effect ef] -> Effect ef
  ShuffleLibrary :: ZOPlayer -> Effect 'OneShot
  StatDelta :: ZOCreature -> Power -> Toughness -> Effect 'Continuous
  Tap :: ZO 'ZBattlefield OTPermanent -> Effect 'OneShot
  Untap :: ZO 'ZBattlefield OTPermanent -> Effect 'OneShot
  Until :: Elect 'Post Event OTPlayer -> Effect 'Continuous -> Effect 'Continuous
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
    GainAbility{} -> 12
    LoseAbility{} -> 13
    PutOntoBattlefield{} -> 14
    Sacrifice{} -> 15
    SearchLibrary{} -> 16
    Sequence{} -> 17
    ShuffleLibrary{} -> 18
    StatDelta{} -> 19
    Tap{} -> 20
    Untap{} -> 21
    Until{} -> 22
    WithList{} -> 23

----------------------------------------

data Elect (p :: PrePost) (el :: Type) (ot :: Type) :: Type where
  ActivePlayer :: (ZOPlayer -> Elect p el ot) -> Elect p el ot
  -- TODO: Add `IsZO zone ot` witness and change `'ZBattlefield` to `zone`.
  All :: IsOT ot => WithMaskedObjects 'ZBattlefield (Elect p el ot) -> Elect p el ot
  -- TODO: Disallow `Choose` for some types of `el` using a witness arg, in particular Event and EventListener
  Choose ::
    (IsPrePost p, Typeable el, IsZO zone ot) =>
    ZOPlayer ->
    WithMaskedObject zone (Elect p el ot) ->
    Elect p el ot
  ChooseOption :: (IsUser u, IsNat n) => ZOPlayer -> NatList u n Condition -> (Variable (Fin u n) -> Elect p el ot) -> Elect p el ot
  Condition :: Condition -> Elect p Condition ot
  ControllerOf :: IsZO zone OTAny => ZO zone OTAny -> (ZOPlayer -> Elect p el ot) -> Elect p el ot
  Cost :: Cost ot -> Elect 'Pre (Cost ot) ot -- XXX: can this constructor be removed?
  Effect :: Typeable ef => [Effect ef] -> Elect 'Post (Effect ef) ot
  Elect :: Typeable el => Elect 'Post el ot -> ElectPrePost el ot
  ElectActivated :: IsZO zone ot => ActivatedAbility zone ot -> Elect 'Pre (ActivatedAbility zone ot) ot
  ElectCard :: CardFacet ot -> Elect 'Pre (CardFacet ot) ot
  ElectCase :: Case (Elect p el ot) -> Elect p el ot
  Event :: Event -> Elect 'Post Event ot
  If :: Condition -> Elect 'Post el ot -> Else el ot -> Elect 'Post el ot -- NOTE: It is probably correct to allow this constructor with CardTypeDef usage in order to encode split cards and such.
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
    ZOPlayer ->
    WithMaskedObject zone (Elect 'Pre el ot) ->
    Elect 'Pre el ot
  VariableFromPower :: ZOCreature -> (Variable Int -> Elect 'Post el ot) -> Elect 'Post el ot
  VariableInt :: (Variable Int -> Elect 'Pre el ot) -> Elect 'Pre el ot
  deriving (Typeable)

instance ConsIndex (Elect p el ot) where
  consIndex = \case
    ActivePlayer{} -> 1
    All{} -> 2
    Choose{} -> 3
    ChooseOption{} -> 4
    Condition{} -> 5
    ControllerOf{} -> 6
    Cost{} -> 7
    Effect{} -> 8
    Elect{} -> 9
    ElectActivated{} -> 10
    ElectCard{} -> 11
    ElectCase{} -> 12
    Event{} -> 13
    If{} -> 14
    Listen{} -> 15
    Random{} -> 16
    Target{} -> 17
    VariableFromPower{} -> 18
    VariableInt{} -> 19

type ElectPrePost el ot = Elect 'Pre (Elect 'Post el ot) ot

----------------------------------------

data Else (el :: Type) (ot :: Type) :: Type where
  ElseCost :: (el ~ Cost ot) => Elect 'Post el ot -> Else el ot
  ElseEffect :: (el ~ Effect 'OneShot) => Elect 'Post el ot -> Else el ot
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

----------------------------------------

data Enchant (zone :: Zone) (ot :: Type) :: Type where
  Enchant :: IsZO zone ot => WithLinkedObject zone (Elect 'Post (Effect 'Continuous)) ot -> Enchant zone ot
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

-- XXX: This should use 'Pre/PrePost instead of 'Post? e.g. [Flametongue Kavu]
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

newtype List a = List [a]
  deriving (Functor, Typeable)

instance Applicative List where
  pure = List . pure
  List f <*> List x = List $ f <*> x

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
  ControlledBy :: IsOT ot => ZOPlayer -> Requirement 'ZBattlefield ot
  ControlsA :: IsOT ot => Requirement 'ZBattlefield ot -> Requirement zone OTPlayer
  HasAbility :: IsZO zone ot => WithThis zone Ability ot -> Requirement zone ot -- Non-unique differing representations will not be considered the same
  HasLandType :: IsZO zone OTLand => LandType -> Requirement zone OTLand
  Is :: IsZO zone ot => WAny ot -> ZO zone ot -> Requirement zone ot
  IsTapped :: IsOT ot => WPermanent ot -> Requirement 'ZBattlefield ot
  Not :: IsZO zone ot => Requirement zone ot -> Requirement zone ot
  OfColors :: IsZO zone ot => Colors -> Requirement zone ot -- needs `WCard a` witness
  OwnedBy :: IsZO zone ot => ZOPlayer -> Requirement zone ot
  PlayerPays :: IsZO zone OTPlayer => Cost OTPlayer -> Requirement zone OTPlayer
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
    ControlledBy{} -> 1
    ControlsA{} -> 2
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

data SomeCard (ot :: Type) :: Type where
  SomeCard :: (ot ~ OTN x, IsSpecificCard ot) => SomeCard ot -> SomeCard ot
  SomeDoubleSidedCard1 :: (ot ~ OTN x, ot' ~ OTN y, Inst2 IsSpecificCard ot ot') => SomeCard ot -> SomeCard ot' -> SomeCard ot
  SomeDoubleSidedCard2 :: (ot ~ OTN x, ot' ~ OTN y, Inst2 IsSpecificCard ot ot') => SomeCard ot' -> SomeCard ot -> SomeCard ot
  SomeSplitCard1 :: (ot ~ OTN x, ot' ~ OTN y, Inst2 IsSpecificCard ot ot') => SomeCard ot -> SomeCard ot' -> SomeCard ot
  SomeSplitCard2 :: (ot ~ OTN x, ot' ~ OTN y, Inst2 IsSpecificCard ot ot') => SomeCard ot' -> SomeCard ot -> SomeCard ot

type SomeToken = Some Token

type SomeCardOrToken ot = Either (SomeCard ot) (SomeToken ot)

mapSome ::
  forall liftOT liftOT' ot.
  Some liftOT ot ->
  (forall ot'. liftOT ot' -> Maybe (liftOT' ot')) ->
  Maybe (Some liftOT' ot)
mapSome some f = case some of
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

fromSome ::
  forall liftOT ot x.
  Some liftOT ot ->
  (forall ot'. liftOT ot' -> x) ->
  x
fromSome some f = case some of
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

----------------------------------------

data StaticAbility (zone :: Zone) (ot :: Type) :: Type where
  As :: (ot ~ OTN x, IsOT ot) => Elect 'Post EventListener ot -> StaticAbility 'ZBattlefield ot -- 603.6d: not a triggered ability
  -- XXX: BestowPre and BestowPost
  Bestow :: ot ~ OTEnchantmentCreature => Elect 'Pre (Cost ot) ot -> Enchant 'ZBattlefield OTCreature -> StaticAbility 'ZBattlefield ot
  FirstStrike :: ot ~ OTCreature => StaticAbility 'ZBattlefield ot
  Flying :: ot ~ OTCreature => StaticAbility 'ZBattlefield ot
  Fuse :: IsOT ot => StaticAbility 'ZHand (ot, ot) -- TODO: Add witness or constraint for OTInstant or OTSorcery
  Haste :: ot ~ OTCreature => StaticAbility 'ZBattlefield ot
  StaticContinuous :: (ot ~ OTN x, IsOT ot) => Elect 'Post (Effect 'Continuous) ot -> StaticAbility 'ZBattlefield ot -- 611.3
  -- XXX: SuspendPre and SuspendPost
  Suspend :: (ot ~ OTN x, IsOT ot) => Int -> Elect 'Pre (Cost ot) ot -> StaticAbility 'ZBattlefield ot -- PositiveInt
  deriving (Typeable)

instance ConsIndex (StaticAbility zone ot) where
  consIndex = \case
    As{} -> 1
    Bestow{} -> 2
    FirstStrike{} -> 3
    Flying{} -> 4
    Fuse{} -> 5
    Haste{} -> 6
    StaticContinuous{} -> 7
    Suspend{} -> 8

----------------------------------------

data Token (ot :: Type) :: Type where
  Token :: (ot ~ OTN x, IsSpecificCard ot) => WPermanent ot -> Card ot -> Token ot
  deriving (Typeable)

instance ConsIndex (Token ot) where
  consIndex = \case
    Token{} -> 1

instance HasCardName (Token ot) where
  getCardName = \case
    Token _ card -> getCardName card

----------------------------------------

-- https://www.mtgsalvation.com/forums/magic-fundamentals/magic-rulings/magic-rulings-archives/611601-whenever-what-does-it-mean?comment=3
-- https://www.reddit.com/r/magicTCG/comments/asmecb/noob_question_difference_between_as_and_when/
data TriggeredAbility (zone :: Zone) (ot :: Type) :: Type where
  When :: IsZO 'ZBattlefield ot => Elect 'Post EventListener ot -> TriggeredAbility 'ZBattlefield ot
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

data WithList (ret :: Type) (zone :: Zone) (ot :: Type) where
  CountOf :: (IsZO zone ot, Typeable ret) => List (ZO zone ot) -> (Variable Int -> ret) -> WithList ret zone ot
  Each :: (IsZO zone ot, Typeable ret) => List (ZO zone ot) -> (ZO zone ot -> ret) -> WithList ret zone ot
  SuchThat :: (IsZO zone ot, Typeable ret) => [Requirement zone ot] -> WithList ret zone ot -> WithList ret zone ot

instance ConsIndex (WithList ret zone ot) where
  consIndex = \case
    CountOf{} -> 1
    Each{} -> 2
    SuchThat{} -> 3

----------------------------------------

data WithMaskedObject (zone :: Zone) (liftedOT :: Type) :: Type where
  Masked1 ::
    (Typeable liftedOT, IsOT (OT1 a), Inst1 IsObjectType a) =>
    [Requirement zone (OT1 a)] ->
    (ZO zone (OT1 a) -> liftedOT) ->
    WithMaskedObject zone liftedOT
  Masked2 ::
    (Typeable liftedOT, IsOT (OT2 a b), Inst2 IsObjectType a b) =>
    [Requirement zone (OT2 a b)] ->
    (ZO zone (OT2 a b) -> liftedOT) ->
    WithMaskedObject zone liftedOT
  Masked3 ::
    (Typeable liftedOT, IsOT (OT3 a b c), Inst3 IsObjectType a b c) =>
    [Requirement zone (OT3 a b c)] ->
    (ZO zone (OT3 a b c) -> liftedOT) ->
    WithMaskedObject zone liftedOT
  Masked4 ::
    (Typeable liftedOT, IsOT (OT4 a b c d), Inst4 IsObjectType a b c d) =>
    [Requirement zone (OT4 a b c d)] ->
    (ZO zone (OT4 a b c d) -> liftedOT) ->
    WithMaskedObject zone liftedOT
  Masked5 ::
    (Typeable liftedOT, IsOT (OT5 a b c d e), Inst5 IsObjectType a b c d e) =>
    [Requirement zone (OT5 a b c d e)] ->
    (ZO zone (OT5 a b c d e) -> liftedOT) ->
    WithMaskedObject zone liftedOT
  Masked6 ::
    (Typeable liftedOT, IsOT (OT6 a b c d e f), Inst6 IsObjectType a b c d e f) =>
    [Requirement zone (OT6 a b c d e f)] ->
    (ZO zone (OT6 a b c d e f) -> liftedOT) ->
    WithMaskedObject zone liftedOT
  deriving (Typeable)

instance ConsIndex (WithMaskedObject zone liftedOT) where
  consIndex = \case
    Masked1{} -> 1
    Masked2{} -> 2
    Masked3{} -> 3
    Masked4{} -> 4
    Masked5{} -> 5
    Masked6{} -> 6

----------------------------------------

data WithMaskedObjects (zone :: Zone) (liftedOT :: Type) :: Type where
  Maskeds1 ::
    (Typeable liftedOT, IsOT (OT1 a), Inst1 IsObjectType a) =>
    [Requirement zone (OT1 a)] ->
    (List (ZO zone (OT1 a)) -> liftedOT) ->
    WithMaskedObjects zone liftedOT
  Maskeds2 ::
    (Typeable liftedOT, IsOT (OT2 a b), Inst2 IsObjectType a b) =>
    [Requirement zone (OT2 a b)] ->
    (List (ZO zone (OT2 a b)) -> liftedOT) ->
    WithMaskedObjects zone liftedOT
  Maskeds3 ::
    (Typeable liftedOT, IsOT (OT3 a b c), Inst3 IsObjectType a b c) =>
    [Requirement zone (OT3 a b c)] ->
    (List (ZO zone (OT3 a b c)) -> liftedOT) ->
    WithMaskedObjects zone liftedOT
  Maskeds4 ::
    (Typeable liftedOT, IsOT (OT4 a b c d), Inst4 IsObjectType a b c d) =>
    [Requirement zone (OT4 a b c d)] ->
    (List (ZO zone (OT4 a b c d)) -> liftedOT) ->
    WithMaskedObjects zone liftedOT
  Maskeds5 ::
    (Typeable liftedOT, IsOT (OT5 a b c d e), Inst5 IsObjectType a b c d e) =>
    [Requirement zone (OT5 a b c d e)] ->
    (List (ZO zone (OT5 a b c d e)) -> liftedOT) ->
    WithMaskedObjects zone liftedOT
  Maskeds6 ::
    (Typeable liftedOT, IsOT (OT6 a b c d e f), Inst6 IsObjectType a b c d e f) =>
    [Requirement zone (OT6 a b c d e f)] ->
    (List (ZO zone (OT6 a b c d e f)) -> liftedOT) ->
    WithMaskedObjects zone liftedOT
  deriving (Typeable)

instance ConsIndex (WithMaskedObjects zone liftedOT) where
  consIndex = \case
    Maskeds1{} -> 1
    Maskeds2{} -> 2
    Maskeds3{} -> 3
    Maskeds4{} -> 4
    Maskeds5{} -> 5
    Maskeds6{} -> 6

----------------------------------------

data WithThis (zone :: Zone) (liftOT :: Type -> Type) (ot :: Type) :: Type where
  This1 ::
    (IsOT (OT1 a), Inst1 IsObjectType a) =>
    (ZO zone (OT1 a) -> liftOT (OT1 a)) ->
    WithThis zone liftOT (OT1 a)
  This2 ::
    (IsOT (OT2 a b), Inst2 IsObjectType a b) =>
    -- TODO: Add a additional full (ZO zone (OT2 a b)) to the input tuple?
    -- TODO: Introduce a `This ot` record type to access its constituents.
    --       Prolly can also add ToObjectN instances (cool!).
    ((ZO zone (OT1 a), ZO zone (OT1 b)) -> liftOT (OT2 a b)) ->
    WithThis zone liftOT (OT2 a b)
  deriving (Typeable)

instance ConsIndex (WithThis zone liftOT ot) where
  consIndex = \case
    This1{} -> 1
    This2{} -> 2

type WithThisActivated zone ot = WithThis zone (Elect 'Pre (ActivatedAbility zone ot)) ot

type WithThisOneShot = WithThis 'ZStack (Elect 'Post (Effect 'OneShot))

type WithThisTriggered zone ot = WithThis zone (TriggeredAbility zone) ot

----------------------------------------

type YourDirect liftOT ot = ZOPlayer -> liftOT ot

type YourElected liftOT ot = ZOPlayer -> Elect 'Pre (liftOT ot) ot

data YourCardFacet (ot :: Type) :: Type where
  YourArtifact :: OTArtifact ~ ot => YourDirect CardFacet ot -> YourCardFacet ot
  YourArtifactCreature :: OTArtifactCreature ~ ot => YourDirect CardFacet ot -> YourCardFacet ot
  YourArtifactLand :: OTArtifactLand ~ ot => YourDirect CardFacet ot -> YourCardFacet ot
  YourCreature :: OTCreature ~ ot => YourDirect CardFacet ot -> YourCardFacet ot
  YourEnchantment :: OTEnchantment ~ ot => YourDirect CardFacet ot -> YourCardFacet ot
  YourEnchantmentCreature :: OTEnchantmentCreature ~ ot => YourDirect CardFacet ot -> YourCardFacet ot
  YourInstant :: OTInstant ~ ot => YourElected CardFacet ot -> YourCardFacet ot
  YourLand :: OTLand ~ ot => YourDirect CardFacet ot -> YourCardFacet ot
  YourPlaneswalker :: OTPlaneswalker ~ ot => YourDirect CardFacet ot -> YourCardFacet ot
  YourSorcery :: OTSorcery ~ ot => YourElected CardFacet ot -> YourCardFacet ot

instance ConsIndex (YourCardFacet ot) where
  consIndex = \case
    YourArtifact{} -> 1
    YourArtifactCreature{} -> 2
    YourArtifactLand{} -> 3
    YourCreature{} -> 4
    YourEnchantment{} -> 5
    YourEnchantmentCreature{} -> 6
    YourInstant{} -> 7
    YourLand{} -> 8
    YourPlaneswalker{} -> 9
    YourSorcery{} -> 10
