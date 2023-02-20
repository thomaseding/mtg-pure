{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use ++" #-}

module MtgPure.Model.Permanent (
  Permanent (..),
  Tapped (..),
  Flipped (..),
  Face (..),
  Phased (..),
  cardToPermanent,
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Artifact (Artifact (..))
import safe MtgPure.Model.Colors (Colors)
import safe MtgPure.Model.Creature (Creature (..))
import safe MtgPure.Model.Damage (Damage, Damage' (..))
import safe MtgPure.Model.Enchantment (AnyEnchantmentType (..), Enchantment (..))
import safe MtgPure.Model.Land (Land (..))
import safe MtgPure.Model.Object.OTNAliases (
  OTNArtifact,
  OTNArtifactCreature,
  OTNArtifactLand,
  OTNCreature,
  OTNEnchantment,
  OTNEnchantmentCreature,
  OTNLand,
  OTNPermanent,
  OTNPlaneswalker,
 )
import safe MtgPure.Model.Planeswalker (Planeswalker (..))
import safe MtgPure.Model.Recursive (
  AnyCard,
  AnyToken,
  CardFacet (..),
  CardFacet' (..),
  SomeOT (..),
  SomeTerm (..),
  SomeZone,
  WithThisAbility,
 )
import safe MtgPure.Model.Variable (Var (NoVar))

data Tapped = Tapped | Untapped
  deriving (Eq, Ord, Show, Typeable)

data Flipped = Flipped | Unflipped
  deriving (Eq, Ord, Show, Typeable)

data Face = FaceUp | FaceDown
  deriving (Eq, Ord, Show, Typeable)

data Phased = PhasedIn | PhasedOut
  deriving (Eq, Ord, Show, Typeable)

data Permanent :: Type where
  Permanent ::
    { permanentAbilities :: [SomeOT (SomeZone WithThisAbility) OTNPermanent]
    , permanentArtifact :: Maybe Artifact
    , permanentCard :: Either AnyCard AnyToken -- SomeCardOrToken OTNPermanent
    , permanentCardFacet :: SomeOT CardFacet OTNPermanent
    , permanentColors :: Colors
    , permanentCreature :: Maybe Creature
    , permanentCreatureDamage :: Damage 'NoVar -- 120.6
    , permanentEnchantment :: Maybe Enchantment
    , permanentFace :: Face
    , permanentFlipped :: Flipped
    , permanentLand :: Maybe Land
    , permanentPhased :: Phased
    , permanentPlaneswalker :: Maybe Planeswalker
    , permanentSummoningSickness :: Bool
    , permanentTapped :: Tapped
    } ->
    Permanent
  deriving (Typeable)

getColors :: CardFacet ot -> Colors
getColors = \case
  ArtifactFacet
    { artifact_colors = colors
    } -> colors
  ArtifactCreatureFacet
    { artifactCreature_colors = colors
    } -> colors
  ArtifactLandFacet
    {
    } -> mempty
  CreatureFacet
    { creature_colors = colors
    } -> colors
  EnchantmentFacet
    { enchantment_colors = colors
    } -> colors
  EnchantmentCreatureFacet
    { enchantmentCreature_colors = colors
    } -> colors
  InstantFacet
    { instant_colors = colors
    } -> colors
  LandFacet
    {
    } -> mempty
  PlaneswalkerFacet
    { planeswalker_colors = colors
    } -> colors
  SorceryFacet
    { sorcery_colors = colors
    } -> colors

someArtifact :: liftOT OTNArtifact -> SomeOT liftOT OTNPermanent
someArtifact = Some5a . SomeArtifact

someCreature :: liftOT OTNCreature -> SomeOT liftOT OTNPermanent
someCreature = Some5b . SomeCreature

someEnchantment :: liftOT OTNEnchantment -> SomeOT liftOT OTNPermanent
someEnchantment = Some5c . SomeEnchantment

someLand :: liftOT OTNLand -> SomeOT liftOT OTNPermanent
someLand = Some5d . SomeLand

somePlaneswalker :: liftOT OTNPlaneswalker -> SomeOT liftOT OTNPermanent
somePlaneswalker = Some5e . SomePlaneswalker

someArtifactCreature :: liftOT OTNArtifactCreature -> SomeOT liftOT OTNPermanent
someArtifactCreature = Some5ab . SomeArtifactCreature

someArtifactLand :: liftOT OTNArtifactLand -> SomeOT liftOT OTNPermanent
someArtifactLand = Some5ad . SomeArtifactLand

someEnchantmentCreature :: liftOT OTNEnchantmentCreature -> SomeOT liftOT OTNPermanent
someEnchantmentCreature = Some5bc . SomeEnchantmentCreature

-- | Usage requirement: The provided facets must actually be part of the provided AnyCard.
cardToPermanent ::
  AnyCard ->
  CardFacet ot ->
  CardFacet' ot ->
  Maybe Permanent
cardToPermanent card facet facet' = case viewPermanentFacet facet of
  Nothing -> Nothing
  Just someFacet ->
    Just
      Permanent
        { permanentAbilities = permanentAbilitiesOf facet'
        , permanentArtifact = facetToArtifact facet
        , permanentCard = Left card
        , permanentCardFacet = someFacet
        , permanentColors = getColors facet
        , permanentCreature = facetToCreature facet facet'
        , permanentCreatureDamage = Damage 0
        , permanentEnchantment = facetToEnchantment facet
        , permanentFace = FaceUp
        , permanentFlipped = Unflipped
        , permanentLand = facetToLand facet
        , permanentPhased = PhasedIn
        , permanentPlaneswalker = facetToPlaneswalker facet'
        , permanentSummoningSickness = True
        , permanentTapped = Untapped
        }

viewPermanentFacet :: CardFacet ot -> Maybe (SomeOT CardFacet OTNPermanent)
viewPermanentFacet facet = case facet of
  InstantFacet{} -> Nothing
  SorceryFacet{} -> Nothing
  ArtifactFacet{} -> Just $ someArtifact facet
  CreatureFacet{} -> Just $ someCreature facet
  EnchantmentFacet{} -> Just $ someEnchantment facet
  LandFacet{} -> Just $ someLand facet
  PlaneswalkerFacet{} -> Just $ somePlaneswalker facet
  ArtifactCreatureFacet{} -> Just $ someArtifactCreature facet
  ArtifactLandFacet{} -> Just $ someArtifactLand facet
  EnchantmentCreatureFacet{} -> Just $ someEnchantmentCreature facet

permanentAbilitiesOf :: CardFacet' ot -> [SomeOT (SomeZone WithThisAbility) OTNPermanent]
permanentAbilitiesOf facet = case facet of
  InstantFacet'{} -> []
  SorceryFacet'{} -> []
  ArtifactFacet'{} -> map someArtifact $ artifact_abilities facet
  CreatureFacet'{} -> map someCreature $ creature_abilities facet
  EnchantmentFacet'{} -> map someEnchantment $ enchantment_abilities facet
  LandFacet'{} -> map someLand $ land_abilities facet
  PlaneswalkerFacet'{} -> map somePlaneswalker $ planeswalker_abilities facet
  ArtifactCreatureFacet'{} ->
    concat
      [ map someArtifact $ artifactCreature_artifactAbilities facet
      , map someCreature $ artifactCreature_creatureAbilities facet
      , map someArtifactCreature $ artifactCreature_artifactCreatureAbilities facet
      ]
  ArtifactLandFacet'{} ->
    concat
      [ map someArtifact $ artifactLand_artifactAbilities facet
      , map someLand $ artifactLand_landAbilities facet
      , map someArtifactLand $ artifactLand_artifactLandAbilities facet
      ]
  EnchantmentCreatureFacet'{} ->
    concat
      [ map someEnchantment $ enchantmentCreature_enchantmentAbilities facet
      , map someCreature $ enchantmentCreature_creatureAbilities facet
      , map someEnchantmentCreature $ enchantmentCreature_enchantmentCreatureAbilities facet
      ]

facetToArtifact :: CardFacet ot -> Maybe Artifact
facetToArtifact facet = case facet of
  ArtifactFacet{} ->
    Just
      Artifact
        { artifactTypes = artifact_artifactTypes facet
        }
  ArtifactCreatureFacet{} ->
    Just
      Artifact
        { artifactTypes = artifactCreature_artifactTypes facet
        }
  ArtifactLandFacet{} ->
    Just
      Artifact
        { artifactTypes = artifactLand_artifactTypes facet
        }
  _ -> Nothing

facetToCreature :: CardFacet ot -> CardFacet' ot -> Maybe Creature
facetToCreature facet _facet' = case facet of
  CreatureFacet{} ->
    Just
      Creature
        { creatureTypes = creature_creatureTypes facet
        , creaturePower = creature_power facet
        , creatureToughness = creature_toughness facet
        }
  ArtifactCreatureFacet{} ->
    Just
      Creature
        { creatureTypes = artifactCreature_creatureTypes facet
        , creaturePower = artifactCreature_power facet
        , creatureToughness = artifactCreature_toughness facet
        }
  EnchantmentCreatureFacet{} ->
    Just
      Creature
        { creatureTypes = enchantmentCreature_creatureTypes facet
        , creaturePower = enchantmentCreature_power facet
        , creatureToughness = enchantmentCreature_toughness facet
        }
  _ -> Nothing

facetToEnchantment :: CardFacet ot -> Maybe Enchantment
facetToEnchantment facet = case facet of
  EnchantmentFacet{} ->
    Just
      Enchantment
        { enchantmentTypes = map AnyEnchantmentType $ enchantment_enchantmentTypes facet
        }
  EnchantmentCreatureFacet{} ->
    Just
      Enchantment
        { enchantmentTypes = map AnyEnchantmentType $ enchantmentCreature_enchantmentTypes facet
        }
  _ -> Nothing

facetToLand :: CardFacet ot -> Maybe Land
facetToLand facet = case facet of
  LandFacet{} ->
    Just
      Land
        { landTypes = land_landTypes facet
        }
  ArtifactLandFacet{} ->
    Just
      Land
        { landTypes = artifactLand_landTypes facet
        }
  _ -> Nothing

facetToPlaneswalker :: CardFacet' ot -> Maybe Planeswalker
facetToPlaneswalker facet' = case facet' of
  PlaneswalkerFacet'{} ->
    Just
      Planeswalker
        { planeswalkerLoyalty = planeswalker_loyalty facet'
        }
  _ -> Nothing
