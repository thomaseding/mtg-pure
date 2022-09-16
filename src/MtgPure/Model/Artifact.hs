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

module MtgPure.Model.Artifact (
  Artifact (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.ArtifactType (ArtifactType)
import safe MtgPure.Model.ObjectType.Kind (OTArtifact)
import safe MtgPure.Model.Recursive (Ability)

data Artifact :: Type where
  Artifact ::
    { artifactTypes :: [ArtifactType]
    , artifactAbilities :: [Ability OTArtifact]
    } ->
    Artifact
  deriving (Typeable)
