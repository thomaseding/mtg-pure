{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module MtgPure.Model.Recursive.Tree (
  TreeM,
  TreeConfig (..),
  runTreeM,
) where

import safe qualified Control.Monad.State.Strict as State
import safe qualified Data.DList as DList
import safe Data.Inst (
  Inst10,
  Inst11,
  Inst12,
  Inst2,
  Inst3,
  Inst4,
  Inst5,
  Inst6,
  Inst7,
  Inst8,
  Inst9,
 )
import safe Data.Kind (Type)
import safe qualified Data.List as List
import safe qualified Data.Map.Strict as Map
import safe Data.Maybe (catMaybes)
import safe Data.Nat (Fin (..), IsNat, NatList (..))
import safe Data.Proxy (Proxy (Proxy))
import safe Data.String (IsString (..))
import safe Data.Typeable (TypeRep, Typeable, typeOf, typeRep)
import safe MtgPure.Model.ArtifactType (ArtifactType)
import safe MtgPure.Model.BasicLandType (BasicLandType)
import safe MtgPure.Model.CardName (CardName (CardName), HasCardName (..))
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.Colors (Colors (..))
import safe MtgPure.Model.CreatureType (CreatureType)
import safe MtgPure.Model.Damage (Damage, Damage' (..))
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.LandType (LandType (..))
import safe MtgPure.Model.Loyalty (Loyalty)
import safe MtgPure.Model.Mana.Mana (Mana (..))
import safe MtgPure.Model.Mana.ManaCost (DynamicManaCost (..), ManaCost (..))
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool (..), ManaPool (..))
import safe MtgPure.Model.Mana.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Object.IndexOT (IndexOT)
import safe MtgPure.Model.Object.IsObjectType (IsObjectType (..))
import safe MtgPure.Model.Object.OTN (
  OT1,
  OT2,
  OT3,
  OT4,
  OT5,
  OT6,
  OTN,
 )
import safe MtgPure.Model.Object.OTNAliases (
  OTNAny,
  OTNArtifact,
  OTNArtifactCreature,
  OTNArtifactLand,
  OTNCreature,
  OTNCreaturePlaneswalker,
  OTNCreaturePlayer,
  OTNCreaturePlayerPlaneswalker,
  OTNDamageSource,
  OTNEnchantment,
  OTNEnchantmentCreature,
  OTNInstant,
  OTNLand,
  OTNPermanent,
  OTNPlaneswalker,
  OTNPlayerPlaneswalker,
  OTNSorcery,
  OTNSpell,
 )
import safe MtgPure.Model.Object.OTN_ (OTN' (..))
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectId (
  ObjectId (ObjectId),
  UntypedObject (..),
  getObjectId,
  pattern DefaultObjectDiscriminant,
 )
import safe MtgPure.Model.Object.ObjectN (
  ON0,
  ON1,
  ON10,
  ON11,
  ON12,
  ON2,
  ON3,
  ON4,
  ON5,
  ON6,
  ON7,
  ON8,
  ON9,
  ObjectN,
 )
import safe MtgPure.Model.Object.ObjectN_ (ObjectN' (..))
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.Singleton.Any (WAny (..))
import safe MtgPure.Model.Object.Singleton.Card (WCard (..))
import safe MtgPure.Model.Object.Singleton.Permanent (WPermanent (..))
import safe MtgPure.Model.Object.Singleton.Spell (WSpell (..))
import safe MtgPure.Model.Object.ViewObjectN (viewOTN')
import safe MtgPure.Model.Object.VisitObjectN (visitObjectN')
import safe MtgPure.Model.Power (Power)
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.PrettyType (PrettyType (..))
import safe MtgPure.Model.Recursive (
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
  Else (..),
  Enchant (..),
  EnchantmentType (..),
  Event,
  EventListener,
  EventListener' (..),
  IsSpecificCard,
  IsUser (..),
  List (..),
  Requirement (..),
  SetCard (SetCard),
  SetToken (SetToken),
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
 )
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.Toughness (Toughness)
import safe MtgPure.Model.Variable (
  Variable (..),
  VariableId,
  VariableId' (..),
  getVariableId,
 )
import safe MtgPure.Model.Zone (IsZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (
  IsOTN,
  IsZO,
  ZO,
  ZoneObject (..),
  toZone,
 )
import safe Prelude hiding (showList)

----------------------------------------

-- This is useful to have so it's easy to walk the tree in various ways with straightforward recursion.
-- No need for users to handle continuations or generate variables.
--
-- Example uses:
--  * Show record style
--  * Show functional style
--  * Simple linting (prolly better to write using the real data types instead of this tree type.)
--  * Lets printers name variables nicer since it has the full tree.
--  * Generate JSON
--  * Pretty-printing
--  * Easier control of use of parens vs dollar sign
data family Tree (a :: Type) :: Type

data instance Tree (Ability ot) where
  TreeActivated :: IsZO zone ot => Tree (WithThisActivated zone ot) -> Tree (Ability ot)
  TreeStatic :: (IsZone zone, IndexOT ot) => Tree (StaticAbility zone ot) -> Tree (Ability ot)
  TreeTriggered :: IsZO zone ot => Tree (WithThisTriggered zone ot) -> Tree (Ability ot)

data instance Tree (ActivatedAbility zone ot) where
  TreeAbility ::
    IsZO zone ot =>
    { treeActivated_cost :: Tree (Cost ot)
    , treeActivated_effect :: Tree (Elect 'Post (Effect 'OneShot) ot)
    } ->
    Tree (ActivatedAbility zone ot)
  TreeCycling :: (ot ~ OTN x, IsOTN ot) => Tree (Cost ot) -> Tree (ActivatedAbility 'ZHand ot)

data instance Tree AnyCard where
  TreeAnyCard1 :: (ot ~ OTN x, IsSpecificCard ot) => Tree (Card ot) -> Tree AnyCard
  TreeAnyCard2 :: (ot1 ~ OTN x, IsSpecificCard ot1, ot2 ~ OTN y, IsSpecificCard ot2) => Tree (Card ot1) -> Tree (Card ot2) -> Tree AnyCard

data instance Tree AnyToken where
  TreeAnyToken :: IsSpecificCard ot => Tree (Token ot) -> Tree AnyToken

data instance Tree (Card ot) where
  TreeCard :: (ot ~ OTN x, IsSpecificCard ot) => CardName -> Tree (YourCardFacet ot) -> Tree (Card ot)
  TreeDoubleFacedCard :: (ot1 ~ OTN x, IsSpecificCard ot1, ot2 ~ OTN y, IsSpecificCard ot2) => Tree (Card ot1) -> Tree (Card ot2) -> Tree (Card (ot1, ot2))
  TreeSplitCard ::
    (ot1 ~ OTN x, IsSpecificCard ot1, ot2 ~ OTN y, IsSpecificCard ot2) =>
    { treeSplitCard_card1 :: Tree (Card ot1)
    , treeSplitCard_card2 :: Tree (Card ot2)
    , treeSplitCard_abilities :: Tree [Ability (ot1, ot2)]
    } ->
    Tree (Card (ot1, ot2))

data instance Tree (CardFacet ot) where
  TreeArtifactFacet ::
    { treeArtifact_colors :: Tree [Color]
    , treeArtifact_cost :: Tree (Cost OTNArtifact)
    , treeArtifact_artifactTypes :: Tree [ArtifactType]
    , treeArtifact_creatureTypes :: Tree [CreatureType]
    , treeArtifact_abilities :: Tree [Ability OTNArtifact]
    } ->
    Tree (CardFacet OTNArtifact)
  TreeArtifactCreatureFacet ::
    { treeArtifactCreature_colors :: Tree [Color]
    , treeArtifactCreature_cost :: Tree (Cost OTNArtifactCreature)
    , treeArtifactCreature_artifactTypes :: Tree [ArtifactType]
    , treeArtifactCreature_creatureTypes :: Tree [CreatureType]
    , treeArtifactCreature_power :: Tree Power
    , treeArtifactCreature_toughness :: Tree Toughness
    , treeArtifactCreature_artifactAbilities :: Tree [Ability OTNArtifact]
    , treeArtifactCreature_creatureAbilities :: Tree [Ability OTNCreature]
    , treeArtifactCreature_artifactCreatureAbilities :: Tree [Ability OTNArtifactCreature]
    } ->
    Tree (CardFacet OTNArtifactCreature)
  TreeArtifactLandFacet ::
    { treeArtifactLand_colors :: Tree [Color]
    , treeArtifactLand_cost :: Tree (Cost OTNArtifactLand)
    , treeArtifactLand_artifactTypes :: Tree [ArtifactType]
    , treeArtifactLand_creatureTypes :: Tree [CreatureType]
    , treeArtifactLand_landTypes :: Tree [LandType]
    , treeArtifactLand_artifactAbilities :: Tree [Ability OTNArtifact]
    , treeArtifactLand_landAbilities :: Tree [Ability OTNArtifactLand]
    , treeArtifactLand_artifactLandAbilities :: Tree [Ability OTNArtifactLand]
    } ->
    Tree (CardFacet OTNArtifactLand)
  TreeCreatureFacet ::
    { treeCreature_colors :: Tree [Color]
    , treeCreature_cost :: Tree (Cost OTNCreature)
    , treeCreature_creatureTypes :: Tree [CreatureType]
    , treeCreature_power :: Tree Power
    , treeCreature_toughness :: Tree Toughness
    , treeCreature_abilities :: Tree [Ability OTNCreature]
    } ->
    Tree (CardFacet OTNCreature)
  TreeEnchantmentFacet ::
    { treeEnchantment_colors :: Tree [Color]
    , treeEnchantment_cost :: Tree (Cost OTNEnchantment)
    , treeEnchantment_creatureTypes :: Tree [CreatureType]
    , treeEnchantment_abilities :: Tree [Ability OTNEnchantment]
    } ->
    Tree (CardFacet OTNEnchantment)
  TreeEnchantmentCreatureFacet ::
    { treeEnchantmentCreature_colors :: Tree [Color]
    , treeEnchantmentCreature_cost :: Tree (Cost OTNEnchantmentCreature)
    , treeEnchantmentCreature_creatureTypes :: Tree [CreatureType]
    , treeEnchantmentCreature_power :: Tree Power
    , treeEnchantmentCreature_toughness :: Tree Toughness
    , treeEnchantmentCreature_creatureAbilities :: Tree [Ability OTNCreature]
    , treeEnchantmentCreature_enchantmentAbilities :: Tree [Ability OTNEnchantment]
    , treeEnchantmentCreature_enchantmentCreatureAbilities :: Tree [Ability OTNEnchantmentCreature]
    } ->
    Tree (CardFacet OTNEnchantmentCreature)
  TreeInstantFacet ::
    { treeInstant_colors :: Tree [Color]
    , treeInstant_cost :: Tree (Cost OTNInstant)
    , treeInstant_creatureTypes :: Tree [CreatureType]
    , treeInstant_abilities :: Tree [Ability OTNInstant]
    } ->
    Tree (CardFacet OTNInstant)
  TreeLandFacet ::
    { treeLand_colors :: Tree [Color]
    , treeLand_cost :: Tree (Cost OTNLand)
    , treeLand_creatureTypes :: Tree [CreatureType]
    , treeLand_landTypes :: Tree [LandType]
    , treeLand_abilities :: Tree [Ability OTNLand]
    } ->
    Tree (CardFacet OTNLand)
  TreePlaneswalkerFacet ::
    { treePlaneswalker_colors :: Tree [Color]
    , treePlaneswalker_cost :: Tree (Cost OTNPlaneswalker)
    , treePlaneswalker_creatureTypes :: Tree [CreatureType]
    , treePlaneswalker_loyalty :: Tree Loyalty
    , treePlaneswalker_abilities :: Tree [Ability OTNPlaneswalker]
    } ->
    Tree (CardFacet OTNPlaneswalker)
  TreeSorceryFacet ::
    { treeSorcery_colors :: Tree [Color]
    , treeSorcery_cost :: Tree (Cost OTNSorcery)
    , treeSorcery_creatureTypes :: Tree [CreatureType]
    , treeSorcery_abilities :: Tree [Ability OTNSorcery]
    } ->
    Tree (CardFacet OTNSorcery)

data instance Tree (Case x) where
  TreeCaseFin ::
    (IsUser u, IsNat n) =>
    { treeCaseFin :: Tree (Variable (Fin u n))
    , treeOfFin :: Tree (NatList u n x)
    } ->
    Tree (Case x)

data instance Tree Condition where
  TreeCAnd :: Tree [Condition] -> Tree Condition
  TreeCNot :: Tree Condition -> Tree Condition
  TreeCOr :: Tree [Condition] -> Tree Condition
  TreeSatisfies :: IsZO zone ot => Tree (ActivatedAbility zone ot) -> Tree Condition

data instance Tree (Variable a) where
  TreeVariable ::
    { treeVariableBaseName :: String
    , treeVariableId :: VariableId
    } ->
    Tree (Variable a)

-----------------------

data TreeConfig = TreeConfig
  { treeConfig_ :: ()
  , -- | This is needed because cards can be mutually recursive with other cards and/or tokens.
    -- In the general case, recursion can only be determined by card name, since comparing
    -- infinite value types is not possible.
    treeConfig_maxCardDepth :: Maybe Int
  }

data TreeM a

runTreeM :: TreeM a -> a
runTreeM = undefined
