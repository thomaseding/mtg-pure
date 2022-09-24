{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
import safe MtgPure.Model.Creature (Creature)
import safe MtgPure.Model.Damage (Damage, Damage' (..))
import safe MtgPure.Model.Land (Land)
import safe MtgPure.Model.Object (Object (..), ObjectType (..))
import safe MtgPure.Model.ObjectType.Kind (OTPermanent)
import safe MtgPure.Model.Recursive (
  Card (..),
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

cardToPermanent :: Object 'OTPlayer -> Card () -> Maybe Permanent
cardToPermanent owner opaqueCard = case opaqueCard of
  Card _name wCard _def -> case wCard of
  TribalCard _name wCard _def -> case wCard of
  InstantCard _card -> Nothing
  SorceryCard _card -> Nothing
  ArtifactCard card -> Just $ go $ Some5a $ SomeArtifact card
  CreatureCard card -> Just $ go $ Some5b $ SomeCreature card
  EnchantmentCard card -> Just $ go $ Some5c $ SomeEnchantment card
  LandCard card -> Just $ go $ Some5d $ SomeLand card
  PlaneswalkerCard card -> Just $ go $ Some5e $ SomePlaneswalker card
  ArtifactCreatureCard card -> Just $ go $ Some5ab $ SomeArtifactCreature card
  EnchantmentCreatureCard card -> Just $ go $ Some5bc $ SomeEnchantmentCreature card
 where
  go :: SomeCard OTPermanent -> Permanent
  go someCard =
    Permanent
      { permanentArtifact = Nothing
      , permanentCard = Left someCard
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
