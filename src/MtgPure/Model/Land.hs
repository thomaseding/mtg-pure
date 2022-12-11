{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Land (
  Land (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.LandType (LandType)

data Land :: Type where
  Land ::
    { landTypes :: [LandType]
    } ->
    Land
  deriving (Typeable)
