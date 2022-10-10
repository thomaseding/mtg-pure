{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
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
import safe MtgPure.Model.Land (Land)
import safe MtgPure.Model.Object (OT, Object (..), ObjectType (..))
import safe MtgPure.Model.ObjectType.Kind (OTPermanent)
import safe MtgPure.Model.Recursive (
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
    { permanentArtifact :: Maybe Artifact
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
  ot ~ OT otk =>
  Object 'OTPlayer ->
  Card ot ->
  CardFacet ot ->
  Maybe Permanent
cardToPermanent owner card def = cardToPermanent' owner card def def

cardToPermanent' ::
  ot ~ OT otk =>
  Object 'OTPlayer ->
  Card ot ->
  CardFacet ot ->
  CardFacet ot ->
  Maybe Permanent
cardToPermanent' owner card topDef bottomDef = case bottomDef of
  SorceryFacet{} -> Nothing
  InstantFacet{} -> Nothing
  --
  ArtifactFacet{} -> Just $ go (Some5a $ SomeArtifact card) (Some5a $ SomeArtifact topDef)
  CreatureFacet{} -> Just $ go (Some5b $ SomeCreature card) (Some5b $ SomeCreature topDef)
  EnchantmentFacet{} -> Just $ go (Some5c $ SomeEnchantment card) (Some5c $ SomeEnchantment topDef)
  LandFacet{} -> Just $ go (Some5d $ SomeLand card) (Some5d $ SomeLand topDef)
  PlaneswalkerFacet{} -> Just $ go (Some5e $ SomePlaneswalker card) (Some5e $ SomePlaneswalker topDef)
  ArtifactCreatureFacet{} -> Just $ go (Some5ab $ SomeArtifactCreature card) (Some5ab $ SomeArtifactCreature topDef)
  ArtifactLandFacet{} -> Just $ go (Some5ad $ SomeArtifactLand card) (Some5ad $ SomeArtifactLand topDef)
  EnchantmentCreatureFacet{} -> Just $ go (Some5bc $ SomeEnchantmentCreature card) (Some5bc $ SomeEnchantmentCreature topDef)
 where
  go :: SomeCard OTPermanent -> Some CardFacet OTPermanent -> Permanent
  go someCard someFacet =
    Permanent
      { permanentArtifact = Nothing
      , permanentCard = Left someCard
      , permanentCardFacet = someFacet
      , permanentColors = getColors topDef
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
