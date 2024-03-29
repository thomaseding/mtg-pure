{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Stack (
  Stack (..),
  StackObject (..),
  stackObjectToZo0,
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)
import safe MtgPure.Model.Object.OTN (OT0)
import safe MtgPure.Model.Object.OTNAliases (
  OTNActivatedOrTriggeredAbility,
  OTNSpell,
 )
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0)
import safe MtgPure.Model.ZoneObject.ZoneObject (ZO)

data StackObject :: Type where
  StackAbility :: ZO 'ZStack OTNActivatedOrTriggeredAbility -> StackObject
  StackSpell :: ZO 'ZStack OTNSpell -> StackObject
  deriving (Typeable)

newtype Stack :: Type where
  Stack :: {unStack :: [StackObject]} -> Stack
  deriving (Typeable)

stackObjectToZo0 :: StackObject -> ZO 'ZStack OT0
stackObjectToZo0 = \case
  StackAbility zo -> toZO0 zo
  StackSpell zo -> toZO0 zo
