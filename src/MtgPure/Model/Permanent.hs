{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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
  Permanents,
  Tapped (..),
  Flipped (..),
  Face (..),
  Phased (..),
) where

import safe Data.Kind (Type)
import safe qualified Data.Map as Map
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Artifact (Artifact)
import safe MtgPure.Model.Creature (Creature)
import safe MtgPure.Model.Damage (Damage)
import safe MtgPure.Model.Land (Land)
import safe MtgPure.Model.ObjectType (OT0)
import safe MtgPure.Model.ObjectType.Kind (OTPermanent)
import safe MtgPure.Model.Recursive (SomeCardOrToken)
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject (OPlayer, ZO)

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
    { permanentCard :: SomeCardOrToken OTPermanent
    , permanentController :: OPlayer
    , permanentSummoningSickness :: Bool
    , permanentTapped :: Tapped
    , permanentFlipped :: Flipped
    , permanentFace :: Face
    , permanentPhased :: Phased
    , permanentOwner :: OPlayer
    , permanentDamage :: Damage -- 120.6
    , permanentArtifact :: Maybe Artifact
    , permanentCreature :: Maybe Creature
    , permanentLand :: Maybe Land
    } ->
    Permanent
  deriving (Typeable)

type Permanents = Map.Map (ZO 'ZBattlefield OT0) Permanent
