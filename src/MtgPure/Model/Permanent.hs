{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

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
import safe MtgPure.Model.Artifact (Artifact)
import safe MtgPure.Model.Colors (Colors)
import safe MtgPure.Model.Creature (Creature)
import safe MtgPure.Model.Damage (Damage, Damage' (..))
import safe MtgPure.Model.Land (Land (..))
import safe MtgPure.Model.Object (Object (..))
import safe MtgPure.Model.ObjectType (ObjectType (..))
import safe MtgPure.Model.ObjectType.Kind (OTPermanent)
import safe MtgPure.Model.Recursive (
  Ability,
  Card (..),
  CardFacet (..),
  Some (..),
  SomeCard,
  SomeCardOrToken,
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
    , permanentCard :: SomeCardOrToken OTPermanent
    , permanentCardFacet :: Some CardFacet OTPermanent
    , permanentColors :: Colors
    , permanentController :: Object 'OTPlayer
    , permanentCreature :: Maybe Creature
    , permanentCreatureDamage :: Damage 'NoVar -- 120.6
    , permanentEnchantment :: Maybe () -- TODO
    , permanentFace :: Face
    , permanentFlipped :: Flipped
    , permanentLand :: Maybe Land
    , permanentOwner :: Object 'OTPlayer
    , permanentPhased :: Phased
    , permanentPlaneswalker :: Maybe () -- TODO
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

cardToPermanent ::
  Object 'OTPlayer ->
  Card ot ->
  CardFacet ot ->
  Maybe Permanent
cardToPermanent owner card facet = case facet of
  SorceryFacet{} -> Nothing
  InstantFacet{} -> Nothing
  --
  ArtifactFacet{} ->
    let some = Some5a . SomeArtifact
     in Just $ go (some card) (some facet)
  CreatureFacet{} ->
    let some = Some5b . SomeCreature
     in Just $ go (some card) (some facet)
  EnchantmentFacet{} ->
    let some = Some5c . SomeEnchantment
     in Just $ go (some card) (some facet)
  LandFacet{} ->
    let some = Some5d . SomeLand
     in Just $
          (go (some card) (some facet))
            { permanentAbilities = map some $ land_abilities facet
            , permanentLand = Just Land{landTypes = land_landTypes facet}
            }
  PlaneswalkerFacet{} ->
    let some = Some5e . SomePlaneswalker
     in Just $ go (some card) (some facet)
  ArtifactCreatureFacet{} ->
    let some = Some5ab . SomeArtifactCreature
     in Just $ go (some card) (some facet)
  ArtifactLandFacet{} ->
    let some = Some5ad . SomeArtifactLand
     in Just $ go (some card) (some facet)
  EnchantmentCreatureFacet{} ->
    let some = Some5bc . SomeEnchantmentCreature
     in Just $ go (some card) (some facet)
 where
  go :: SomeCard OTPermanent -> Some CardFacet OTPermanent -> Permanent
  go someCard someFacet =
    Permanent
      { permanentAbilities = mempty
      , permanentArtifact = Nothing
      , permanentCard = Left someCard
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
