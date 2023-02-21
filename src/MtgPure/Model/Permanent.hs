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
  CardCharacteristic (..),
  CardSpec (..),
  SomeOT (..),
  SomeTerm (..),
  SomeZone,
  WithThisAbility,
 )
import safe MtgPure.Model.Supertype (Supertype)
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
    , permanentCardCharacteristic :: SomeOT CardCharacteristic OTNPermanent
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
    , permanentSupertypes :: [SomeOT Supertype OTNPermanent]
    , permanentTapped :: Tapped
    } ->
    Permanent
  deriving (Typeable)

getColors :: CardCharacteristic ot -> Colors
getColors = \case
  ArtifactCharacteristic
    { artifact_colors = colors
    } -> colors
  ArtifactCreatureCharacteristic
    { artifactCreature_colors = colors
    } -> colors
  ArtifactLandCharacteristic
    {
    } -> mempty
  CreatureCharacteristic
    { creature_colors = colors
    } -> colors
  EnchantmentCharacteristic
    { enchantment_colors = colors
    } -> colors
  EnchantmentCreatureCharacteristic
    { enchantmentCreature_colors = colors
    } -> colors
  InstantCharacteristic
    { instant_colors = colors
    } -> colors
  LandCharacteristic
    {
    } -> mempty
  PlaneswalkerCharacteristic
    { planeswalker_colors = colors
    } -> colors
  SorceryCharacteristic
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

-- | Usage requirement: The provided characters must actually be part of the provided AnyCard.
cardToPermanent ::
  AnyCard ->
  CardCharacteristic ot ->
  CardSpec ot ->
  Maybe Permanent
cardToPermanent card character spec = case viewPermanentCharacteristic character of
  Nothing -> Nothing
  Just someCharacteristic ->
    Just
      Permanent
        { permanentAbilities = permanentAbilitiesOf spec
        , permanentArtifact = characterToArtifact character
        , permanentCard = Left card
        , permanentCardCharacteristic = someCharacteristic
        , permanentColors = getColors character
        , permanentCreature = characterToCreature character spec
        , permanentCreatureDamage = Damage 0
        , permanentEnchantment = characterToEnchantment character
        , permanentFace = FaceUp
        , permanentFlipped = Unflipped
        , permanentLand = characterToLand character
        , permanentPhased = PhasedIn
        , permanentPlaneswalker = characterToPlaneswalker spec
        , permanentSummoningSickness = True
        , permanentSupertypes = characterToSupertypes character
        , permanentTapped = Untapped
        }

viewPermanentCharacteristic :: CardCharacteristic ot -> Maybe (SomeOT CardCharacteristic OTNPermanent)
viewPermanentCharacteristic character = case character of
  InstantCharacteristic{} -> Nothing
  SorceryCharacteristic{} -> Nothing
  ArtifactCharacteristic{} -> Just $ someArtifact character
  CreatureCharacteristic{} -> Just $ someCreature character
  EnchantmentCharacteristic{} -> Just $ someEnchantment character
  LandCharacteristic{} -> Just $ someLand character
  PlaneswalkerCharacteristic{} -> Just $ somePlaneswalker character
  ArtifactCreatureCharacteristic{} -> Just $ someArtifactCreature character
  ArtifactLandCharacteristic{} -> Just $ someArtifactLand character
  EnchantmentCreatureCharacteristic{} -> Just $ someEnchantmentCreature character

permanentAbilitiesOf :: CardSpec ot -> [SomeOT (SomeZone WithThisAbility) OTNPermanent]
permanentAbilitiesOf spec = case spec of
  InstantSpec{} -> []
  SorcerySpec{} -> []
  ArtifactSpec{} -> map someArtifact $ artifact_abilities spec
  CreatureSpec{} -> map someCreature $ creature_abilities spec
  EnchantmentSpec{} -> map someEnchantment $ enchantment_abilities spec
  LandSpec{} -> map someLand $ land_abilities spec
  PlaneswalkerSpec{} -> map somePlaneswalker $ planeswalker_abilities spec
  ArtifactCreatureSpec{} ->
    concat
      [ map someArtifact $ artifactCreature_artifactAbilities spec
      , map someCreature $ artifactCreature_creatureAbilities spec
      , map someArtifactCreature $ artifactCreature_artifactCreatureAbilities spec
      ]
  ArtifactLandSpec{} ->
    concat
      [ map someArtifact $ artifactLand_artifactAbilities spec
      , map someLand $ artifactLand_landAbilities spec
      , map someArtifactLand $ artifactLand_artifactLandAbilities spec
      ]
  EnchantmentCreatureSpec{} ->
    concat
      [ map someEnchantment $ enchantmentCreature_enchantmentAbilities spec
      , map someCreature $ enchantmentCreature_creatureAbilities spec
      , map someEnchantmentCreature $ enchantmentCreature_enchantmentCreatureAbilities spec
      ]

characterToSupertypes :: CardCharacteristic ot -> [SomeOT Supertype OTNPermanent]
characterToSupertypes character = case character of
  InstantCharacteristic{} -> []
  SorceryCharacteristic{} -> []
  ArtifactCharacteristic{} -> map someArtifact $ artifact_supertypes character
  ArtifactCreatureCharacteristic{} -> map someArtifactCreature $ artifactCreature_supertypes character
  ArtifactLandCharacteristic{} -> map someArtifactLand $ artifactLand_supertypes character
  CreatureCharacteristic{} -> map someCreature $ creature_supertypes character
  EnchantmentCharacteristic{} -> map someEnchantment $ enchantment_supertypes character
  EnchantmentCreatureCharacteristic{} -> map someEnchantmentCreature $ enchantmentCreature_supertypes character
  LandCharacteristic{} -> map someLand $ land_supertypes character
  PlaneswalkerCharacteristic{} -> map somePlaneswalker $ planeswalker_supertypes character

characterToArtifact :: CardCharacteristic ot -> Maybe Artifact
characterToArtifact character = case character of
  ArtifactCharacteristic{} ->
    Just
      Artifact
        { artifactTypes = artifact_artifactTypes character
        }
  ArtifactCreatureCharacteristic{} ->
    Just
      Artifact
        { artifactTypes = artifactCreature_artifactTypes character
        }
  ArtifactLandCharacteristic{} ->
    Just
      Artifact
        { artifactTypes = artifactLand_artifactTypes character
        }
  _ -> Nothing

characterToCreature :: CardCharacteristic ot -> CardSpec ot -> Maybe Creature
characterToCreature character _spec = case character of
  CreatureCharacteristic{} ->
    Just
      Creature
        { creatureTypes = creature_creatureTypes character
        , creaturePower = creature_power character
        , creatureToughness = creature_toughness character
        }
  ArtifactCreatureCharacteristic{} ->
    Just
      Creature
        { creatureTypes = artifactCreature_creatureTypes character
        , creaturePower = artifactCreature_power character
        , creatureToughness = artifactCreature_toughness character
        }
  EnchantmentCreatureCharacteristic{} ->
    Just
      Creature
        { creatureTypes = enchantmentCreature_creatureTypes character
        , creaturePower = enchantmentCreature_power character
        , creatureToughness = enchantmentCreature_toughness character
        }
  _ -> Nothing

characterToEnchantment :: CardCharacteristic ot -> Maybe Enchantment
characterToEnchantment character = case character of
  EnchantmentCharacteristic{} ->
    Just
      Enchantment
        { enchantmentTypes = map AnyEnchantmentType $ enchantment_enchantmentTypes character
        }
  EnchantmentCreatureCharacteristic{} ->
    Just
      Enchantment
        { enchantmentTypes = map AnyEnchantmentType $ enchantmentCreature_enchantmentTypes character
        }
  _ -> Nothing

characterToLand :: CardCharacteristic ot -> Maybe Land
characterToLand character = case character of
  LandCharacteristic{} ->
    Just
      Land
        { landTypes = land_landTypes character
        }
  ArtifactLandCharacteristic{} ->
    Just
      Land
        { landTypes = artifactLand_landTypes character
        }
  _ -> Nothing

characterToPlaneswalker :: CardSpec ot -> Maybe Planeswalker
characterToPlaneswalker spec = case spec of
  PlaneswalkerSpec{} ->
    Just
      Planeswalker
        { planeswalkerLoyalty = planeswalker_loyalty spec
        }
  _ -> Nothing
