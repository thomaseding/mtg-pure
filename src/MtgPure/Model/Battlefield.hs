{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Battlefield (
  Battlefield (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Recursive (AnyCard)
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject (ZO)

newtype Battlefield :: Type where
  Battlefield :: [ZO 'ZBattlefield AnyCard] -> Battlefield
  deriving (Typeable)
