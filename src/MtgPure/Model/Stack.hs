{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Stack (
  Stack (..),
  StackObject (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object.OTKind (
  OTActivatedOrTriggeredAbility,
  OTSpell,
 )
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (ZO)

data StackObject :: Type where
  StackAbility :: ZO 'ZStack OTActivatedOrTriggeredAbility -> StackObject
  StackSpell :: ZO 'ZStack OTSpell -> StackObject
  deriving (Typeable)

newtype Stack :: Type where
  Stack :: {unStack :: [StackObject]} -> Stack
  deriving (Typeable)
