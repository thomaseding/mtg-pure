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
  OTArtifact,
  OTArtifactCreature,
  OTArtifactLand,
  OTCreature,
  OTEnchantment,
  OTEnchantmentCreature,
  OTLand,
  OTPermanent,
  OTPlaneswalker,
 )
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Planeswalker (Planeswalker (..))
import safe MtgPure.Model.Recursive (
  Ability,
  AnyCard,
  AnyToken,
  CardFacet (..),
  Some (..),
  SomeTerm (..),
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
    { permanentAbilities :: [Some Ability OTPermanent]
    , permanentArtifact :: Maybe Artifact
    , permanentCard :: Either AnyCard AnyToken -- SomeCardOrToken OTPermanent
    , permanentCardFacet :: Some CardFacet OTPermanent
    , permanentColors :: Colors
    , permanentController :: Object 'OTPlayer
    , permanentCreature :: Maybe Creature
    , permanentCreatureDamage :: Damage 'NoVar -- 120.6
    , permanentEnchantment :: Maybe Enchantment
    , permanentFace :: Face
    , permanentFlipped :: Flipped
    , permanentLand :: Maybe Land
    , permanentOwner :: Object 'OTPlayer
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

someArtifact :: liftOT OTArtifact -> Some liftOT OTPermanent
someArtifact = Some5a . SomeArtifact

someCreature :: liftOT OTCreature -> Some liftOT OTPermanent
someCreature = Some5b . SomeCreature

someEnchantment :: liftOT OTEnchantment -> Some liftOT OTPermanent
someEnchantment = Some5c . SomeEnchantment

someLand :: liftOT OTLand -> Some liftOT OTPermanent
someLand = Some5d . SomeLand

somePlaneswalker :: liftOT OTPlaneswalker -> Some liftOT OTPermanent
somePlaneswalker = Some5e . SomePlaneswalker

someArtifactCreature :: liftOT OTArtifactCreature -> Some liftOT OTPermanent
someArtifactCreature = Some5ab . SomeArtifactCreature

someArtifactLand :: liftOT OTArtifactLand -> Some liftOT OTPermanent
someArtifactLand = Some5ad . SomeArtifactLand

someEnchantmentCreature :: liftOT OTEnchantmentCreature -> Some liftOT OTPermanent
someEnchantmentCreature = Some5bc . SomeEnchantmentCreature

-- | Usage requirement: The provided CardFacet must actually be part of the provided AnyCard.
cardToPermanent ::
  Object 'OTPlayer ->
  AnyCard ->
  CardFacet ot ->
  Maybe Permanent
cardToPermanent owner card facet = case facet of
  InstantFacet{} -> Nothing
  SorceryFacet{} -> Nothing
  --
  ArtifactFacet{} ->
    let some = someArtifact
     in Just
          (go card (some facet))
            { permanentAbilities = map some $ artifact_abilities facet
            , permanentArtifact =
                Just
                  Artifact
                    { artifactTypes = artifact_artifactTypes facet
                    }
            }
  CreatureFacet{} ->
    let some = someCreature
     in Just
          (go card (some facet))
            { permanentAbilities = map some $ creature_abilities facet
            , permanentCreature =
                Just
                  Creature
                    { creatureTypes = creature_creatureTypes facet
                    , creaturePower = creature_power facet
                    , creatureToughness = creature_toughness facet
                    }
            }
  EnchantmentFacet{} ->
    let some = someEnchantment
     in Just
          (go card (some facet))
            { permanentAbilities = map some $ enchantment_abilities facet
            , permanentEnchantment =
                Just
                  Enchantment
                    { enchantmentTypes = map AnyEnchantmentType $ enchantment_enchantmentTypes facet
                    }
            }
  LandFacet{} ->
    let some = someLand
     in Just
          (go card (some facet))
            { permanentAbilities = map some $ land_abilities facet
            , permanentLand =
                Just
                  Land
                    { landTypes = land_landTypes facet
                    }
            }
  PlaneswalkerFacet{} ->
    let some = somePlaneswalker
     in Just
          (go card (some facet))
            { permanentAbilities = map some $ planeswalker_abilities facet
            , permanentPlaneswalker =
                Just
                  Planeswalker
                    { planeswalkerLoyalty = planeswalker_loyalty facet
                    }
            }
  ArtifactCreatureFacet{} ->
    let some = someArtifactCreature
     in Just
          (go card (some facet))
            { permanentAbilities =
                concat
                  [ map someArtifact $ artifactCreature_artifactAbilities facet
                  , map someCreature $ artifactCreature_creatureAbilities facet
                  , map someArtifactCreature $ artifactCreature_artifactCreatureAbilities facet
                  ]
            , permanentArtifact =
                Just
                  Artifact
                    { artifactTypes = artifactCreature_artifactTypes facet
                    }
            , permanentCreature =
                Just
                  Creature
                    { creatureTypes = artifactCreature_creatureTypes facet
                    , creaturePower = artifactCreature_power facet
                    , creatureToughness = artifactCreature_toughness facet
                    }
            }
  ArtifactLandFacet{} ->
    let some = someArtifactLand
     in Just
          (go card (some facet))
            { permanentAbilities =
                concat
                  [ map someArtifact $ artifactLand_artifactAbilities facet
                  , map someLand $ artifactLand_landAbilities facet
                  , map someArtifactLand $ artifactLand_artifactLandAbilities facet
                  ]
            , permanentArtifact =
                Just
                  Artifact
                    { artifactTypes = artifactLand_artifactTypes facet
                    }
            , permanentLand =
                Just
                  Land
                    { landTypes = artifactLand_landTypes facet
                    }
            }
  EnchantmentCreatureFacet{} ->
    let some = someEnchantmentCreature
     in Just
          (go card (some facet))
            { permanentAbilities =
                concat
                  [ map someCreature $ enchantmentCreature_creatureAbilities facet
                  , map someEnchantment $ enchantmentCreature_enchantmentAbilities facet
                  , map someEnchantmentCreature $ enchantmentCreature_enchantmentCreatureAbilities facet
                  ]
            , permanentCreature =
                Just
                  Creature
                    { creatureTypes = enchantmentCreature_creatureTypes facet
                    , creaturePower = enchantmentCreature_power facet
                    , creatureToughness = enchantmentCreature_toughness facet
                    }
            , permanentEnchantment =
                Just
                  Enchantment
                    { enchantmentTypes = map AnyEnchantmentType $ enchantmentCreature_enchantmentTypes facet
                    }
            }
 where
  go :: AnyCard -> Some CardFacet OTPermanent -> Permanent
  go anyCard someFacet =
    Permanent
      { permanentAbilities = mempty
      , permanentArtifact = Nothing
      , permanentCard = Left anyCard
      , permanentCardFacet = someFacet
      , permanentColors = getColors facet
      , permanentController = owner
      , permanentCreature = Nothing
      , permanentCreatureDamage = Damage 0
      , permanentEnchantment = Nothing
      , permanentFace = FaceUp
      , permanentFlipped = Unflipped
      , permanentLand = Nothing
      , permanentOwner = owner
      , permanentPhased = PhasedIn
      , permanentPlaneswalker = Nothing
      , permanentSummoningSickness = True
      , permanentTapped = Untapped
      }
