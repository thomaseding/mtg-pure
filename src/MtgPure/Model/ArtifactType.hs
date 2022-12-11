{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.ArtifactType (
  ArtifactType (..),
) where

import safe Data.Typeable (Typeable)

data ArtifactType
  = Gold
  deriving (Eq, Ord, Show, Typeable)
