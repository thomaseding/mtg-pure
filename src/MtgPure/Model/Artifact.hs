{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Artifact (
  Artifact (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.ArtifactType (ArtifactType)

data Artifact :: Type where
  Artifact ::
    { artifactTypes :: [ArtifactType]
    } ->
    Artifact
  deriving (Typeable)
