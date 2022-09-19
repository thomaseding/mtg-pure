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
import safe MtgPure.Model.Damage (Damage (Damage))
import safe MtgPure.Model.Land (Land)
import safe MtgPure.Model.Object (Object (..))
import safe MtgPure.Model.ObjectType (ObjectType (..))
import safe MtgPure.Model.ObjectType.Kind (OTPermanent)
import safe MtgPure.Model.Recursive (Card (..), Some (..), SomeCard, SomeCardOrToken)

data Tapped = Tapped | Untapped
  deriving (Typeable)

data Flipped = Flipped | Unflipped
  deriving (Typeable)

data Face = FaceUp | FaceDown
  deriving (Typeable)

data Phased = PhasedIn | PhasedOut
  deriving (Typeable)

data Permanent :: Type where
  Permanent ::
    { permanentArtifact :: Maybe Artifact
    , permanentCard :: SomeCardOrToken OTPermanent
    , permanentController :: Object 'OTPlayer
    , permanentCreature :: Maybe Creature
    , permanentDamage :: Damage -- 120.6
    , permanentFace :: Face
    , permanentFlipped :: Flipped
    , permanentLand :: Maybe Land
    , permanentOwner :: Object 'OTPlayer
    , permanentPhased :: Phased
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
      , permanentDamage = Damage 0
      , permanentFace = FaceUp
      , permanentFlipped = Unflipped
      , permanentLand = Nothing
      , permanentOwner = owner
      , permanentPhased = PhasedIn
      , permanentSummoningSickness = True
      , permanentTapped = Untapped
      }
