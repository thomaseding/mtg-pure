{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant multi-way if" #-}

-- WIP
module MtgPure.Model.Recursive.Tree (
  TreeM,
  TreeConfig (..),
  runTreeM,
  BuildTree (..),
  buildTree,
  Tree (..),
) where

import safe qualified Control.Monad.State.Strict as State
import safe Data.Kind (Type)
import safe Data.Nat (Fin (..), IsNat, NatList (..))
import safe MtgPure.Model.ArtifactType (ArtifactType)
import safe MtgPure.Model.CardName (CardName)
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.CreatureType (CreatureType)
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.LandType (LandType (..))
import safe MtgPure.Model.Loyalty (Loyalty)
import safe MtgPure.Model.Object.OTN (
  OTN,
 )
import safe MtgPure.Model.Object.OTNAliases (
  OTNArtifact,
  OTNArtifactCreature,
  OTNArtifactLand,
  OTNCreature,
  OTNEnchantment,
  OTNEnchantmentCreature,
  OTNInstant,
  OTNLand,
  OTNPlaneswalker,
  OTNSorcery,
 )
import safe MtgPure.Model.Object.ObjectId (
  ObjectId (ObjectId),
 )
import safe MtgPure.Model.Power (Power)
import safe MtgPure.Model.PrePost (PrePost (..))
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
  IsSpecificCard,
  IsUser,
  SomeZone (..),
  Token,
  WithThisAbility,
  YourCardFacet,
 )
import safe MtgPure.Model.Toughness (Toughness)
import safe MtgPure.Model.Variable (
  Variable (..),
  VariableId,
  VariableId' (..),
 )
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (
  IsOTN,
  IsZO,
 )

--------------------------------------------------------------------------------

data TreeConfig = TreeConfig
  { treeConfig_ :: ()
  , -- | This is needed because cards can be mutually recursive with other cards and/or tokens.
    -- In the general case, recursion can only be determined by card name, since comparing
    -- infinite value types is not possible.
    treeConfig_maxCardDepth :: Maybe Int
  }

-- Don't export this.
data TreeState = TreeState
  { treeState_ :: ()
  , treeState_config :: TreeConfig
  , treeState_cardDepth :: Int
  , nextBoundObjectId :: ObjectId -- e.g. `\target ->`
  , nextBoundVariableId :: VariableId -- e.g. `\x ->`
  }

newtype TreeM a = TreeM {unTreeM :: State.State TreeState a}
  deriving (Functor)

instance Applicative TreeM where
  pure = TreeM . pure
  (<*>) f x = TreeM $ unTreeM f <*> unTreeM x

instance Monad TreeM where
  (>>=) m f = TreeM $ unTreeM m >>= unTreeM . f

instance State.MonadState TreeState TreeM where
  get = TreeM State.get
  put = TreeM . State.put

runTreeM :: TreeConfig -> TreeM a -> a
runTreeM config m = State.evalState (unTreeM m) st
 where
  st =
    TreeState
      { treeState_ = ()
      , treeState_config = config
      , treeState_cardDepth = 0
      , nextBoundObjectId = ObjectId 0
      , nextBoundVariableId = VariableId 0
      }

class BuildTree a where
  buildTreeM :: a -> TreeM (Tree a)

buildTree :: BuildTree a => TreeConfig -> a -> Tree a
buildTree config = runTreeM config . buildTreeM

--------------------------------------------------------------------------------

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
--
-- XXX: Prolly want to keep the `ot` types since IndexOT exists and the strong types are useful.
-- In the future, if there is need, there can be a `UntypedTree` variant that doesn't have the `ot` types.
-- To create an `UntypedTree`, build one of these `Tree ot`s and then convert that to an `UntypedTree`.
-- Each UntypedTree constructor can have a lazy field for the `ot` object types as `[ObjectType]`.
data family Tree (a :: Type) :: Type

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
    , treeSplitCard_abilities :: Tree [SomeZone Ability (ot1, ot2)]
    } ->
    Tree (Card (ot1, ot2))

data instance Tree (CardFacet ot) where
  TreeArtifactFacet ::
    { treeArtifact_colors :: Tree [Color]
    , treeArtifact_cost :: Tree (Cost OTNArtifact)
    , treeArtifact_artifactTypes :: Tree [ArtifactType]
    , treeArtifact_creatureTypes :: Tree [CreatureType]
    , treeArtifact_abilities :: Tree [SomeZone WithThisAbility OTNArtifact]
    } ->
    Tree (CardFacet OTNArtifact)
  TreeArtifactCreatureFacet ::
    { treeArtifactCreature_colors :: Tree [Color]
    , treeArtifactCreature_cost :: Tree (Cost OTNArtifactCreature)
    , treeArtifactCreature_artifactTypes :: Tree [ArtifactType]
    , treeArtifactCreature_creatureTypes :: Tree [CreatureType]
    , treeArtifactCreature_power :: Tree Power
    , treeArtifactCreature_toughness :: Tree Toughness
    , treeArtifactCreature_artifactAbilities :: Tree [SomeZone WithThisAbility OTNArtifact]
    , treeArtifactCreature_creatureAbilities :: Tree [SomeZone WithThisAbility OTNCreature]
    , treeArtifactCreature_artifactCreatureAbilities :: Tree [SomeZone WithThisAbility OTNArtifactCreature]
    } ->
    Tree (CardFacet OTNArtifactCreature)
  TreeArtifactLandFacet ::
    { treeArtifactLand_colors :: Tree [Color]
    , treeArtifactLand_cost :: Tree (Cost OTNArtifactLand)
    , treeArtifactLand_artifactTypes :: Tree [ArtifactType]
    , treeArtifactLand_creatureTypes :: Tree [CreatureType]
    , treeArtifactLand_landTypes :: Tree [LandType]
    , treeArtifactLand_artifactAbilities :: Tree [SomeZone WithThisAbility OTNArtifact]
    , treeArtifactLand_landAbilities :: Tree [SomeZone WithThisAbility OTNArtifactLand]
    , treeArtifactLand_artifactLandAbilities :: Tree [SomeZone WithThisAbility OTNArtifactLand]
    } ->
    Tree (CardFacet OTNArtifactLand)
  TreeCreatureFacet ::
    { treeCreature_colors :: Tree [Color]
    , treeCreature_cost :: Tree (Cost OTNCreature)
    , treeCreature_creatureTypes :: Tree [CreatureType]
    , treeCreature_power :: Tree Power
    , treeCreature_toughness :: Tree Toughness
    , treeCreature_abilities :: Tree [SomeZone WithThisAbility OTNCreature]
    } ->
    Tree (CardFacet OTNCreature)
  TreeEnchantmentFacet ::
    { treeEnchantment_colors :: Tree [Color]
    , treeEnchantment_cost :: Tree (Cost OTNEnchantment)
    , treeEnchantment_creatureTypes :: Tree [CreatureType]
    , treeEnchantment_abilities :: Tree [SomeZone WithThisAbility OTNEnchantment]
    } ->
    Tree (CardFacet OTNEnchantment)
  TreeEnchantmentCreatureFacet ::
    { treeEnchantmentCreature_colors :: Tree [Color]
    , treeEnchantmentCreature_cost :: Tree (Cost OTNEnchantmentCreature)
    , treeEnchantmentCreature_creatureTypes :: Tree [CreatureType]
    , treeEnchantmentCreature_power :: Tree Power
    , treeEnchantmentCreature_toughness :: Tree Toughness
    , treeEnchantmentCreature_creatureAbilities :: Tree [SomeZone WithThisAbility OTNCreature]
    , treeEnchantmentCreature_enchantmentAbilities :: Tree [SomeZone WithThisAbility OTNEnchantment]
    , treeEnchantmentCreature_enchantmentCreatureAbilities :: Tree [SomeZone WithThisAbility OTNEnchantmentCreature]
    } ->
    Tree (CardFacet OTNEnchantmentCreature)
  TreeInstantFacet ::
    { treeInstant_colors :: Tree [Color]
    , treeInstant_cost :: Tree (Cost OTNInstant)
    , treeInstant_creatureTypes :: Tree [CreatureType]
    , treeInstant_abilities :: Tree [SomeZone WithThisAbility OTNInstant]
    } ->
    Tree (CardFacet OTNInstant)
  TreeLandFacet ::
    { treeLand_colors :: Tree [Color]
    , treeLand_cost :: Tree (Cost OTNLand)
    , treeLand_creatureTypes :: Tree [CreatureType]
    , treeLand_landTypes :: Tree [LandType]
    , treeLand_abilities :: Tree [SomeZone WithThisAbility OTNLand]
    } ->
    Tree (CardFacet OTNLand)
  TreePlaneswalkerFacet ::
    { treePlaneswalker_colors :: Tree [Color]
    , treePlaneswalker_cost :: Tree (Cost OTNPlaneswalker)
    , treePlaneswalker_creatureTypes :: Tree [CreatureType]
    , treePlaneswalker_loyalty :: Tree Loyalty
    , treePlaneswalker_abilities :: Tree [SomeZone WithThisAbility OTNPlaneswalker]
    } ->
    Tree (CardFacet OTNPlaneswalker)
  TreeSorceryFacet ::
    { treeSorcery_colors :: Tree [Color]
    , treeSorcery_cost :: Tree (Cost OTNSorcery)
    , treeSorcery_creatureTypes :: Tree [CreatureType]
    , treeSorcery_abilities :: Tree [SomeZone WithThisAbility OTNSorcery]
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
